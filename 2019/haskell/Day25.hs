{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Main where

import Control.Applicative ((<|>), many)
import Control.Lens (makeLenses, (^.),(&),(.~),(%~),(^?!),_Just)
import Control.Monad.State (State,get,put,evalState,state)
import Data.Bifunctor (first)
import Data.Char (chr,ord)
import Data.Foldable (asum, traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Traversable (for)
import Data.Void(Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, errorBundlePretty, runParser, eof, manyTill, try)
import Text.Megaparsec.Char (eol, printChar, string)

import Intcode

-- Line-based, string-based interface to the droid.
-- The function will newline-terminate the input.
-- We panic if the IC doesn't accept the whole line.
-- The maybe tells us when the machine halts.
runToIn' :: IC -> (String, Maybe (String -> IC))
runToIn' ic = case runToIO ic of
  ICOut c ic' -> first (chr (fromInteger c):) $ runToIn' ic'
  ICIn f -> ("", Just $ giveLine f)
  ICHalt _ -> ("", Nothing)
  where giveLine :: (Integer -> IC) -> String -> IC
        giveLine f "" = f $ toInteger $ ord '\n'
        giveLine f (c:cs) =  case runToIO $ f $ toInteger $ ord c of
          ICIn f' -> giveLine f' cs
          _ -> error "giveLine: machine did not want all the input"

newtype StringIC = SIC {runSIC :: String -> (String, Maybe StringIC)}
runToIn :: IC -> (String, Maybe StringIC)
runToIn ic = let (s,x) = runToIn' ic
             in (s,fmap (SIC . (.) runToIn) x)

type Item = String

data Door = N | E | S | W deriving (Eq, Show)

reverseDoor :: Door -> Door
reverseDoor N = S
reverseDoor E = W
reverseDoor S = N
reverseDoor W = E

data Room = Room {_name :: String
                 ,_desc :: String
                 ,_doors :: [Door]
                 ,_items :: [Item]
                 }
  deriving Show
$(makeLenses ''Room)

data Weight = TooHeavy | TooLight
data RejectRoom = AcceptRoom Room -- this is a actual room which we have parsed
                | RejectRoom Weight Room -- hit security checkpoint, and got kicked out, the 'Room' is the one we were kicked into

parseRoom :: String -> Either (ParseErrorBundle String Void) RejectRoom
parseRoom = runParser parseRoom' ""

parseRoom' :: Parsec Void String RejectRoom
parseRoom' = try (AcceptRoom <$> oneRoom)
         <|> uncurry RejectRoom <$> securityThenOneRoom
  where oneRoom = mkRoom <$> nameP <*> descP <*> doorsP <*> (Left <$> itemsP <* promptP <|> Right <$> success) <* eof
        mkRoom n des dor (Left item) = Room n des dor item
        mkRoom n des dor (Right santa) = Room n (unlines [des,santa]) dor []
        securityThenOneRoom = (,) <$ nameP <* descP <* doorsP <*> securityP <*> oneRoom <* eof
        nameP = eol *> eol *> eol *> string "== " *> manyTill printChar (string " ==") <* eol
        descP = manyTill printChar eol <* eol
        doorsP = string "Doors here lead:" *> eol *> parseList directionP <* eol
        directionP = N <$ string "north"
                 <|> E <$ string "east"
                 <|> S <$ string "south"
                 <|> W <$ string "west"
        itemsP = string "Items here:" *> eol *> parseList (many printChar) <* eol
             <|> pure []
        promptP = string "Command?" <* eol
        success = (\l1 l2 l3 -> unlines [l1,l2,l3])
              <$> string "A loud, robotic voice says \"Analysis complete! You may proceed.\" and you enter the cockpit." <* eol
              <*> line
              <*> line
        line = many printChar <* eol
        securityP = string "A loud, robotic voice says \"Alert! Droids on this ship are "
                 *> (TooLight <$ string "heavier" <|> TooHeavy <$ string "lighter")
                 <* string " than the detected value!\" and you are ejected back to the checkpoint."
                 <* eol

parseList :: Parsec Void String p -> Parsec Void String [p]
parseList p = many (string "- " *> p <* eol)

parseInventory :: Parsec Void String [Item]
parseInventory = eol
              *> string "Items in your inventory:"
              *> eol
              *> parseList (many printChar)
              <* eol
              <* string "Command?"
              <* eol
              <* eof


data Game = Game {_traps :: [Item] -- we don't actually need to store this, but it is nice to report traps at the end
                 ,_beforeLastPickup :: Maybe (Game, Item)
                 ,_movesSinceLastPickup :: [Door] -- latest first
                 ,_droid :: StringIC
                 }
$(makeLenses ''Game)

data MoveResult = Ok Game -- success (includes bouncing off the security checkpoint)
                | Trap Game -- noticed last pickup was a trap, have rewound to not pick it up, and then replayed moves
                            -- i.e. success, but have lost an item

-- Returns Nothing if the move failed (i.e. we didn't seem to reach a room - probably we have picked up a trap!)
move' :: Door -> Game -> (MoveResult, RejectRoom)
move' d g = let (r,d') = (g ^. droid) `runSIC` doorString d
                room' = parseRoom r
            in case (room',d') of
                 (Right room@(AcceptRoom _), Just newDroid) -> (Ok $ g & droid .~ newDroid & movesSinceLastPickup %~ (d:), room)
                 (Right room@(RejectRoom _ _), Just newDroid) -> (Ok $ g & droid .~ newDroid, room)
                 (Right room, Nothing) -> (Ok undefined, room) -- This is rather unsafe. It should only happen when we have just gotten
                                                               -- through the security door, and then will never inspect the undefined.
                 _ -> case move' d $ untrap g of -- assume last picked up item was trap
                   (Ok g', r') -> (Trap g', r')
                   (Trap _, _) -> error "Uh oh, multiple traps?" -- If I understand the puzzle correctly, this should not happen!
  where doorString N = "north"
        doorString E = "east"
        doorString S = "south"
        doorString W = "west"
        move'' :: Door -> Game -> Game
        -- errors if the move fails, for use when replaying a series
        -- of moves after un-picking the trap (so we believe this should never fail)
        move'' door game = case move' door game of
          (Ok g', _) -> g'
          _ -> error "Replaying moves failed - our trap detection is buggy!"
        untrap :: Game -> Game
        -- Rewind and replay so the last item was never picked up
        untrap game = let (prior, trap) = game ^?! beforeLastPickup._Just
                          since = game ^. movesSinceLastPickup
                          new = foldr move'' prior since
                      in new & traps %~ (trap:)

-- Do the move, silently dropping old pickups if necessary
move :: Door -> State Game RejectRoom
move d = state $ \g -> case move' d g of
  (Ok g', r) -> (r, g')
  (Trap g', r) -> (r, g')

-- Unfortunately, "apply f, but if it fails then backtrack and retry" doesn't form the bind of a monad
-- so we can't get nice do notation for backtracking. Instead we will do this step manually, using move.

pickup :: Item -> State Game ()
pickup i = do g <- get
              let (r,d') = (g ^. droid) `runSIC` ("take " ++ i)
              let expected = unlines [""
                                     ,"You take the " ++ i ++ "."
                                     ,""
                                     ,"Command?"
                                     ]
              let newStateIfTrap = g & traps %~ (i:)
              if r /= expected
                then put newStateIfTrap
                else case d' of
                       Nothing -> put newStateIfTrap
                       Just droid' -> put $ Game {_traps = g ^. traps
                                                 ,_beforeLastPickup = Just (g, i)
                                                 ,_movesSinceLastPickup = []
                                                 ,_droid = droid'
                                                 }

putdown :: Item -> State Game ()
putdown i = do g <- get
               let (r,d') = (g ^. droid) `runSIC` ("drop " ++ i)
               let expected = unlines [""
                                      ,"You drop the " ++ i ++ "."
                                      ,""
                                      ,"Command?"
                                      ]
               if r /= expected
                 then error "Didn't expect a trap upon dropping"
                 else case d' of
                        Nothing -> error "Didn't expect a trap upon dropping"
                        Just droid' -> put $ g & droid .~ droid'

inventory :: State Game [Item]
inventory = do g <- get
               let (inv', _) = (g ^. droid) `runSIC` "inv"
               case runParser parseInventory "" inv' of
                 Left err -> error (errorBundlePretty err)
                 Right inv -> pure inv

data Transition a = Up a | Down a

-- Gray code expressed as named bit transitions
grayPicks :: [a] -> [Transition a]
grayPicks [] = []
grayPicks (b:bs) = let lower = grayPicks bs
                   in lower ++ [Up b] ++ map flipT (reverse lower)
  where flipT (Up a) = Down a
        flipT (Down a) = Up a

-- Assume the ship is a tree with backedges.
-- Do a preorder traversal of the ship picking up all items, detecting the traps and recording where the exit is.
-- Then go to the exit and search for the set of items that will let us pass.
-- Return whatever text the game prints after passing the exit
parta :: Room -> State Game String
parta r = pickupAll Nothing r >>= \case
            Nothing -> error "Couldn't find security checkpoint"
            Just (ds, d) -> walk ds >> passDoor d
  where pickupAll :: Maybe Door -> Room -> State Game (Maybe ([Door],Door))
        -- preorder traversal; returns the path to the room before the pressure pad and the door to the pad
        pickupAll ent room = do case room ^. items of
                                  [] -> pure ()
                                  [i] -> pickup i
                                  is -> error $ "Did not expect multiple items in a room! Found: " ++ unwords is
                                  -- because then how would we tell which was a trap when we next move?
                                let ret = reverseDoor <$> ent
                                childrenPath <- for (filter ((/=ret).Just) $ room ^. doors) $ \d ->
                                  move d >>= \case AcceptRoom r' -> pickupAll (Just d) r'
                                                   RejectRoom _ _ -> pure (Just ([],d))
                                let path' = asum childrenPath
                                case ent of
                                  Nothing -> pure path'
                                  Just ent' -> move (reverseDoor ent') >> pure (first (ent':) <$> path')
        walk :: [Door] -> State Game ()
        walk = traverse_ move
        passDoor :: Door -> State Game String
        -- We want to drop some subset of our inventory to be the correct weight to get through security
        passDoor d = do inv <- S.fromList <$> inventory
                        go (grayPicks $ S.toList inv) inv S.empty S.empty S.empty
          where go :: [Transition Item] -> S.Set Item -> S.Set Item -> S.Set (S.Set Item) -> S.Set (S.Set Item) -> State Game String
                -- Iterate through subsets using Gray code. Skip testing sets whose answer is determined by
                -- prior knowledge.
                go [] _ _ _ _ = error "passDoor: ran out of options"
                go (p:ps) inv flr tooLight tooHeavy =
                  do tooLightHeavy' <- if inv `S.member` tooLight || flr `S.member` tooHeavy
                                       then pure $ Left (tooLight, tooHeavy)
                                       else move d <&> \case
                                         AcceptRoom r' -> Right $ r' ^. desc
                                         RejectRoom w _ -> Left $ case w of
                                           TooLight -> (tooLight `S.union` S.powerSet inv, tooHeavy)
                                           TooHeavy -> (tooLight, tooHeavy `S.union` S.powerSet flr)
                     case tooLightHeavy' of
                       Right s -> pure s
                       Left (tooLight', tooHeavy') -> case p of
                         Down i -> pickup i *> go ps (S.insert i inv) (S.delete i flr) tooLight' tooHeavy'
                         Up i -> putdown i *> go ps (S.delete i inv) (S.insert i flr) tooLight' tooHeavy'

mkGame :: IC -> Either (ParseErrorBundle String Void) (Room, Game)
mkGame mach = let (r,f) = runToIn mach
                  g = Game {_traps = [], _beforeLastPickup = Nothing, _movesSinceLastPickup = [], _droid = fromJust f}
              in do r' <- parseRoom r
                    case r' of
                      AcceptRoom r'' -> pure (r'', g)
                      RejectRoom _ _ -> error "mkGame: first room can't be checkpoint!"

main :: IO ()
main = do mach <- readIntcode "../data/day25"
          case mkGame mach of
            Left err -> putStr $ errorBundlePretty err
            Right (r,g) -> putStrLn $ evalState (parta r) g
