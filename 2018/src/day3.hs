import qualified Data.Map as M

-- 3a: given a bunch of rectangles, how many square inches are covered by at least two of them
data Claim = Claim {ident :: Int, lt :: (Int,Int), rb :: (Int,Int) }
  deriving Show
left, right, top, bot :: Claim -> Int
left = fst . lt
right = fst . rb
top = snd . lt
bot = snd . rb

instance Eq Claim where
  c == d = ident c == ident d

-- format '#1 @ 2,3: 4x5' means id 1, top left 2,3, width 4, height 5
-- we measure everything from top left corner
day3a_data :: IO [Claim]
day3a_data = map parse <$> lines <$> readFile "../data/3a"
  where parse l = case words l of
                    ['#':id', "@", lt', wh'] ->
                      let id = read id'
                          (l',',':t') = break (==',') lt'
                          l = read l'
                          t = read $ init t'
                          (w','x':h') = break (=='x') wh'
                          w = read w'
                          h = read h'
                      in Claim id (l,t) (l+w-1,t+h-1)

day3a_main :: IO ()
day3a_main = print =<< day3a_solve <$> day3a_data

-- build a map of square inch:#claims on it
day3a_solve :: [Claim] -> Int
day3a_solve cs = M.size $ M.filter (>1) $ M.fromListWith (+) $ concatMap sqrs cs
sqrs (Claim _ (l,t) (r,b)) = [((x,y),1) | x<-[l..r] , y<-[t..b]]


-- 3b: find the exactly one rectangle that doesn't overlap any other
day3b_data :: IO [Claim]
day3b_data = day3a_data

day3b_main :: IO ()
day3b_main = print =<< day3b_solve <$> day3b_data

day3b_solve :: [Claim] -> Claim
day3b_solve cs = case filter (\c -> all (notClash c) cs) cs of
                   [c] -> c
                   _ -> error "not exactly one non-clashing?"
  where notClash c d = c == d || (bot c < top d || bot d < top c) || (right c < left d || right d < left c)
