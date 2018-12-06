import Text.Megaparsec

import Util

-- ?a: ...
day?a_parser :: Parser parsed
day?a_parser = _

day?a_solve :: parsed -> Either err res
day?a_solve = _

day?a_show :: res -> String
day?a_show = _

day?a_main :: IO ()
day?a_main = generic_main "../data/?a" day?a_parser day?a_solve day?a_show


-- ?b: ...
day?b_parser :: Parser parsed
day?b_parser = _

day?b_solve :: parsed -> Either err res
day?b_solve = _

day?b_show :: res -> String
day?b_show = _

day?b_main :: IO ()
day?b_main = generic_main "../data/?b" day?b_parser day?b_solve day?b_show
