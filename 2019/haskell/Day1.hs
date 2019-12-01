module Main where

import Conduit
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)

main :: IO ()
main = do tot <- runConduitRes $ sourceFile "../data/day1a" .| linesUnboundedAsciiC .| mapC (fst . fromJust . readInt) .| mapC toFuel .| sumC
          putStr "Total fuel needed: "
          print tot

toFuel :: Int -> Int
toFuel mass = mass `div` 3 - 2
