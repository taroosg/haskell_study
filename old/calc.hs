mul (i,x) = x * i
calc xs = foldl (+) 0 (map mul (zip [0..] xs))

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

makeString :: Char -> Int -> [Char]
makeString c 0 = []
makeString c n = c : makeString c (n - 1)

-- ["boo","foo","woo"]と["foo","woo"]で組み合わせをつくる．あまりは無視される
makePair xs = zip xs (tail xs)
-- makePair ["boo","foo","woo"]
-- [("boo","foo"),("foo","woo")]

-- ペアが同じかどうかを判定
pairEq (x,y) = x == y
-- 組み合わせをつくり，同じかどうかを連続して判定してリストを作り，長さを算出する．
comlen xs ys = length (takeWhile pairEq (zip xs ys))
-- 文字列と文字列の組を作成し，共通部分の長さと第一要素の文字列を返す
lenstr (xs,ys) = (comlen xs ys, xs)
-- 長さを計算する関数の完成
calcLen = map lenstr

-- 組の最初で比較する関数
compFst (n1,s1) (n2,s2) = compare n1 n2
-- 組の最初で比較した結果の最大値を返す関数
chooseMax = maximumBy compFst

-- chooseMax [(1,"boo"),(3,"foo"),(2,"woo")]
-- (3,"foo")

-- リストの先頭にあるi個の要素をリストとして取り出す
extract (i,xs) = take i xs

-- 必要な関数をまとめる
maxDupStr = extract . chooseMax . calcLen . makePair . sort . tails