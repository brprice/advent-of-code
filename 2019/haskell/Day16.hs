module Main where

import Control.Monad (unless)
import Data.List (foldl', tails)

-- the units digit, where trunc (-17) = 7
trunc :: Integer -> Integer
trunc = (`mod` 10).abs

fft :: [Integer] -> [Integer] -> [Integer]
fft pat dat = zipWith (const go) dat $ map (tail . stretch pat) [1..] -- dat here is unused, except for its length
  where stretch xs n = cycle $ concatMap (replicate n) xs
        go p = trunc $ sum $ zipWith (*) dat p

digitsToInteger :: [Integer] -> Integer
digitsToInteger = foldl' (\n d -> n*10 + d) 0

binom :: Integer -> Integer -> Integer
binom n k = product [n-k+1..n] `div` product [1..k]

-- Use Lucas's theorem to calculate n`choose`k mod p, for p assumed prime
binom_p :: Integer -> Integer -> Integer -> Integer
binom_p n k p = product $ go (digs n) (digs k)
  where digs 0 = [] -- digits in reverse order
        digs m = let (q,r) = quotRem m p
                 in r : digs q
        go [] [] = []
        go (_:nds) [] = 1 : go nds []
        go [] (_:kds) = 0 : go [] kds
        go (nd:nds) (kd:kds) = binom nd kd : go nds kds

-- extGCD a b = (d,m,n) where d = gcd a b = m*a+n*b
extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a b = go a b 1 0 0 1
  where go r0 0 s0 _ t0 _ = (r0,s0,t0)
        go r0 r1 s0 s1 t0 t1 = let (q,r) = quotRem r0 r1
                                   s = s0 - q * s1
                                   t = t0 - q * t1
                               in go r1 r s1 s t1 t

-- Chinese remainder theorem
-- solve x = a mod m, x = b mod n for 0<=x<m*n
-- (for coprime n, m)
crt :: (Integer,Integer) -> (Integer,Integer) -> Integer
crt (a,m) (b,n) = let (g,u,v) = extGCD m n
                  in if g /= 1
                     then error $ unwords ["crt: moduli ",show m,"and",show n,"were not coprime! (gcd = ",show g,")"]
                     else (a*v*n + b*u*m) `mod` (m*n)

-- Binomial coefficients modulo 10
binom_10 :: Integer -> Integer -> Integer
binom_10 n k = crt (binom_p n k 2,2) (binom_p n k 5,5)

main :: IO ()
main = do dat' <- readFile "../data/day16"
          let dat = map (read . (:[])) $ init dat' -- remove trailing newline
          let numFft = 100

          putStr "part a: "
          print $ digitsToInteger $ take 8 $ iterate (fft [0,1,0,-1]) dat !! numFft

          {- Part b: we want a particular 8 digits a decent way into the 100th fft
             of (the input repeated 10 thousand times).
             At least in my input, this is well into the second half.
             Note that since the pattern for the nth output digit is 0^{n-1}1^n...,
             we have that in the second half we are just taking the sum of the tails.
             Note that we can work mod 10, as we don't have to worry about truncating negative numbers.
             So we can work on the tail of the list starting at the offset.
             Consider the list x0,x1,...,xk, and the map f(x) = (x0+x1+...+xk, x1+...+xk, ..., xk)
             (i.e. a generic input, and the fft within the second half).
             Then f^n(x)_i = sum(j=i..n , binom(n-1+j,j)*x_j).
             To see this, consider the tree of additions we are doing to find f^n(x)_i.
             Each path terminates in a leaf which is one of the x_j, and the nodes are the f^m(x)_j for
             m=n,n-1,...,0, and some increasing j starting at i.
             Considering the coefficient of x_k and the grid of m=n..0, j=i..k, we are tracing out
             staircase paths, from (n,i) to (1,k) to (0,k). There are binom(n+k-i-1,n-1) of these.
           -}
          let off = fromIntegral $ digitsToInteger $ take 7 dat
          let datLen = length dat
          let datbLen = 10000 * datLen
          unless (off * 2 >= datbLen) $
            error "part b: offset is not in the second-half of the repeated input!"
          let numFft' = fromIntegral numFft
          let partBTail = take (datbLen - off) $ drop (rem off datLen) $ cycle dat
          let partBBinom = map (\j -> binom_10 (numFft' + j - 1) (numFft' - 1)) [0..]
          let sumMod10 = foldl' (\a x -> (a + x) `mod` 10) 0
          let partB' = sumMod10 . zipWith (\b x -> (b * x) `mod` 10) partBBinom
          let partB = digitsToInteger $ map partB' $ take 8 $ tails partBTail
          putStr "part b: "
          print partB
