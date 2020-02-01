mul (i,x) = x * i

calc :: [Int] -> Int
-- 入力と出力の型があっていないのでコンパイルエラー
calc xs = foldl (+) 0 . zip [0..]