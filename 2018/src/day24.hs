{-# LANGUAGE TupleSections #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.Void

import Util

data Army = Immune | Infect deriving Eq
data Armies = Armies {immune, infect :: [Group]}
data Group = Group {units, hp, dmg, initiative :: Int, typ :: Typ, weak, immun :: [Typ]}
  deriving (Eq, Show)
data Typ = Typ String deriving (Eq, Show)

-- 24a: simulate a fight
day24a_parser :: Parser Armies
day24a_parser = (\imu inf -> Armies imu inf)
             <$  string "Immune System:" <* eol
             <*> army
             <*  eol
             <* string "Infection:" <* eol
             <*> army
  where army = many (parseGroup <* eol)
        parseGroup = (\u hps (im,wk) atk aTyp ini -> Group u hps atk ini aTyp wk im)
                 <$> decimal <* string " units each with " <*> decimal <* string " hit points "
                 <*> immuweak
                 <*  string "with an attack that does " <*> decimal <* string " " <*> atkTyp <* string " damage "
                 <*  string "at initiative " <*> decimal
        immuweak =   (try $ (,) <$ string "(" <*> parseImmu <* string "; " <*> parseWeak <* string ") ")
                 <|> (try $ flip (,) <$ string "(" <*> parseWeak <* string "; " <*> parseImmu <* string ") ")
                 <|> (try $ (,[]) <$ string "(" <*> parseImmu <* string ") ")
                 <|> (try $ ([],) <$ string "(" <*> parseWeak <* string ") ")
                 <|> pure ([],[])
        parseImmu = (:) <$ string "immune to " <*> atkTyp <*> many (string ", " *> atkTyp)
        parseWeak = (:) <$ string "weak to " <*> atkTyp <*> many (string ", " *> atkTyp)
        atkTyp = Typ <$> many letterChar

type Id = Int

-- does one round
fight :: M.Map Id (Army,Group) -> M.Map Id (Army,Group)
fight as = attack as $ select as

maximumOnMay :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOnMay f xs = if null xs then Nothing else Just $ maximumBy (comparing f) xs

effPow :: Group -> Int
effPow g = units g * dmg g

-- 0 if immune, 2 if weak
reduction :: Typ -> Group -> Int
reduction t g = if t `elem` weak g
                then 2
                else if t `elem` immun g
                     then 0
                     else 1

-- who should we target?
select :: M.Map Id (Army,Group) -> M.Map Id Id
select as = snd $ foldl' go (M.assocs as, M.empty) $ sortBy (comparing (Down . effPowInit . grp)) $ M.assocs as
  where grp (_,(_,g)) = g
        effPowInit g = (effPow g, initiative g)
        go (free, targeted) (sid,(a,g)) = case maximumOnMay (potDamOrd g) $ filter ((/=a).arm) free of
                                            Nothing -> (free, targeted)
                                            Just t@(tid,(_,g'))
                                              | potDam g g' == 0 -> (free, targeted) -- can't deal anyone any damage
                                              | otherwise -> (delete t free, M.insert sid tid targeted)
        arm (_,(a,_)) = a
        potDamOrd g (_,(_,g')) = (potDam g g',effPowInit g')
        potDam g g' = effPow g * reduction (typ g) g'

attack :: M.Map Id (Army,Group) -> M.Map Id Id -> M.Map Id (Army,Group)
attack as tgts = go as $ sortBy (comparing (Down . initiative . snd . (M.!) as . fst)) $ M.assocs tgts
  where go state [] = state
        go state ((sId,tId):todo) = let s = snd $ state M.! sId
                                        (at,gt) = state M.! tId
                                        damage = effPow s * reduction (typ s) gt
                                        loss = damage `div` hp gt
                                    in if loss >= units gt
                                       then go (M.delete tId state) $ filter ((/=tId).fst) todo -- entire group dies
                                       else go (M.insert tId (at, gt{units=units gt - loss}) state) todo

-- Left = stalemate
combat :: Armies -> Either (M.Map Id (Army,Group)) (Army,[Group])
combat (Armies immu inf) = let imId = zip [0,2..] $ map (Immune,) immu
                               inId = zip [1,3..] $ map (Infect,) inf
                               as = M.fromList $ imId ++ inId
                           in  fmap foo $ go onlyOneLeft as $ tail $ iterate fight as
  where onlyOneLeft as = let a = fst $ snd $ M.findMin as
                         in all ((==a).fst) as
        go _ _ [] = error "Infinite list ended?"
        go p prev (x:xs) | x == prev = Left x
                         | p x = Right x
                         | otherwise = go p x xs
        foo as = let ((a,g1):as') = M.elems as
                 in (a,g1:map snd as')


day24a_solve :: Armies -> Either String Int
day24a_solve as = case combat as of
                    Left stalemate -> let (immu,inf) = partition ((==Immune).fst) $ M.elems stalemate
                                      in Left $ "Stalemate found, at "
                                          ++ unlines ("Immune:":map (show.snd) immu)
                                          ++ unlines ("Infection:":map (show.snd) inf)
                    Right victory -> Right $ sum $ map units $ snd $ victory

day24a_main :: IO ()
day24a_main = generic_main "../data/24a" day24a_parser day24a_solve show


-- 24b: find smallest boost to immune system that lets it win
-- rubbish linear search:
day24b_solve :: Armies -> Either Void Int
day24b_solve as = pure $ sum $ map units $ snd $ fromRight $ head $ filter ((==(Right Immune)).fmap fst) $ map combat $ boosted
  where boosted = map (boost as) [0..]
        boost (Armies immu infec) n = Armies (map (\g -> g{dmg = dmg g + n}) immu) infec
        fromRight (Right x) = x
        fromRight _ = error "We've checked it is Right already!"

day24b_main :: IO ()
day24b_main = generic_main "../data/24a" day24a_parser day24b_solve show
