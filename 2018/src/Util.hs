module Util where

import Data.Void
import Text.Megaparsec

type Parser r = Parsec Void String r

generic_main :: Show e => FilePath -> Parser a -> (a -> Either e b) -> (b -> String) -> IO ()
generic_main f parser solver printer = (putStrLn . go) =<< (runParser (parser <* eof) f <$> readFile f)
  where go (Left e) = errorBundlePretty e
        go (Right a) = case solver a of
                         Left e -> show e
                         Right b -> printer b


