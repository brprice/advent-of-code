module Main where

type Digit = Int

-- Idea: if we didn't have the range bound, it is easy combinatiorics
-- let's semi-brute force it: brute force until definitely avoided the bounds
-- and then do the combinatorics

-- NB: there are some nice identities about sums of binomial coefficients that apply here
-- (in the sum [countPasswd ... | ...] cases)
-- and would be a nice optimisation, but I expect we will be easily fast enough without them
countPasswd :: Integer -- n: how many digits
            -> Bool -- do we still need a duplicate
            -> Digit -- previous digit (-1 if no previous)
            -> Maybe [Digit] -- upper bound (lexicographic), length n
            -> Maybe [Digit] -- lower bound (lexicographic), length n
            -> Integer -- how many passwords
countPasswd 0 True _ _ _ = 0
countPasswd 0 False _ _ _ = 1
countPasswd n d p Nothing Nothing = let p' = toInteger (max 0 p)
                                        allInc = (9 - p' + n) `choose` n
                                        noDup = if p>=0 then (9-p')`choose`n else (9 - p' + 1)`choose`n
                                    in allInc - if d then noDup else 0
countPasswd n d p Nothing (Just (l:ls)) | p > l = countPasswd n d p Nothing Nothing
                                        | otherwise = countPasswd (n-1) (d && p/=l) l Nothing (Just ls) -- pick l
                                                    + sum [countPasswd (n-1) d x Nothing Nothing | x<-[l+1..9]] -- pick above l, have avoided lower bound
countPasswd n d p (Just (u:us)) lls = let (l,ls) = case lls of
                                            Nothing -> (max 0 p,Nothing)
                                            Just (l':ls') -> if l'<p then (max 0 p,Nothing) else (l',Just ls')
                                            Just [] -> error "Found non-zero length, but lower bound of length zero"
                                      in case compare l u of -- know p<=l
                                           LT -> countPasswd (n-1) (d && p/=l) l Nothing ls -- pick l, have avoided upper bound
                                               + countPasswd (n-1) d u (Just us) Nothing -- pick u, have avoided lower bound
                                               + sum [countPasswd (n-1) d x Nothing Nothing | x <- [l+1..u-1]] -- pick strictly between l and u
                                           EQ -> countPasswd (n-1) (d && p/=l) l (Just us) ls
                                           GT -> 0
countPasswd _ _ _ (Just []) _ = error "Found non-zero length, but upper bound of length zero"
countPasswd _ _ _ _ (Just []) = error "Found non-zero length, but lower bound of length zero"

choose :: Integer -> Integer -> Integer
choose n k = product [n-k+1..n] `div` product [1..k]

digits :: Integer -> [Digit]
digits n | n < 0 = error "digits<0"
         | otherwise = reverse $ go n
  where go 0 = []
        go m = let (q,r) = quotRem m 10
               in fromIntegral r : go q

main :: IO ()
main = let -- puzzle input
           lb = digits 240298
           ub = digits 784956
           len = if length lb == length ub then length lb else error "input lower and upper bounds are of different length"
       in print $ countPasswd (toInteger len) True (-1) (Just ub) (Just lb)
