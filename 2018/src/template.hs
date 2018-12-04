-- ?a: ...
day?a_data :: IO _
day?a_data = readFile "../data/?a"

day?a_main :: IO ()
day?a_main = print =<< day?a_solve <$> day?a_data

day?a_solve :: _ -> _
day?a_solve = _

-- ?b: ...
day?b_data :: IO _
day?b_data = readFile "../data/?b"

day?b_main :: IO ()
day?b_main = print =<< day?b_solve <$> day?b_data

day?b_solve :: _ -> _
day?b_solve = _
