-- main関数が実行されるやつ
-- main = putStrLn "hello, world"
-- main = do { putStrLn "hello" ; putStrLn "world" ; putStrLn "goodbye" }


-- インデントで区切れる
-- main = do putStrLn "hello"
--           putStrLn "world"
--           putStrLn "goodbye"


-- 変数の束縛（再代入はできないので束縛と呼ぶ）
-- m = 123   -- 変数 n を値 123 に束縛
-- n = 456   -- 変数 n を値 456 に束縛

-- main = do print m  -- 出力: 123
--           print n  -- 出力: 456


-- 関数は「関数名 引数 引数」的な感じに書く
-- add x y = x + y
-- main = print (add 2 3)  -- 出力: 5


-- タプルの形式
-- (a, b, c) = (123, 3.14, "hello")
-- main = do
--     print a  -- 出力: 123
--     print b  -- 出力: 3.14
--     print c  -- 出力: "hello"


-- 変数名とタプルの中身をまとめて束縛できる
-- t@(a, b, c) = (123, 3.14, "hello")
-- main = do
--     print a  -- 出力: 123
--     print b  -- 出力: 3.14
--     print c  -- 出力: "hello"
--     print t  -- 出力: (123, 3.14, "hello")


-- caseは順番に評価するので，マッチした後は評価しない
-- getValue defval maybe =
--   case maybe of
--     Nothing -> defval
--     Just x  -> x

-- main = do
--   print (getValue 0 Nothing)   -- 出力: 0
--   print (getValue 0 (Just 5))  -- 出力: 5


-- guardを使って条件式で式を評価できる
-- absMaybe x =
  -- case x of
--     Nothing            -> 0
--     Just x | x < 0     -> -x   -- Just x にマッチし，x < 0 のとき
--            | otherwise -> x    -- Just x にマッチし，x < 0 でないとき

-- main = do
--   print (absMaybe Nothing)      -- 出力: 0
--   print (absMaybe (Just 5))     -- 出力: 5
--   print (absMaybe (Just (-5)))  -- 出力: 5


-- ifは条件式を用いて値を返す．返す値の型は一致している必要がある
-- absolute x = if x < 0 then -x else x

-- main = print (absolute (-5))  -- 出力: 5


-- 上のif文をcase文で書くと以下
-- absolute x =
--   case x of
--     x | x < 0     -> -x
--       | otherwise ->  x

-- main = print (absolute (-5))


-- letで宣言された変数はその関数内でのみ有効．ローカル変数的な感じ
-- 式の前にinをつけないといかん
-- area r =
--   let pi = 3.14
--       square x = x * x
--   in pi * square r

-- main = print (area 10.0)  -- 出力: 314.0


-- whereをつけると変数を後ろに書ける（上の例の書き換え）
-- area r = pi * square r
--   where
--     pi = 3.14
--     square x = x * x

-- main = print (area 10.0)  -- 出力: 314.0


-- 関数は「関数名 引数 引数 ．．．」的な感じ
-- add x y = x + y

-- main = print (add 2 3)  -- 出力: 5
-- 演算子より関数のほうが強い（add 2 3）* 9 的な感じ
-- main = print (add 2 3 * 9) -- 値: 45

-- $を使うと順番をいい感じにできる（下は3 * 9を先に計算する）
-- main = print $ add 2 $ 3 * 9


-- 関数の型は引数 -> 引数 -> 戻り値，という感じ
-- add :: Int -> Int -> Int
-- add x y = x + y

-- mainの戻り値も設定できる
-- main :: IO ()
-- $を使ってaddしてからprint的な感じにしている
-- main = print $ add 2 3


-- forループは存在しないので再起で計算する（階乗の計算）
-- 引数と戻り値の型を定義
-- factorial :: Int -> Int
-- 0だったら1，他だったらもうx-1を引数にしてx * factorial (x - 1)を再度実行
-- factorial x =
--   if x == 0 then 1
--             else x * factorial (x - 1)

-- main = print $ factorial 5  -- 出力: 120

-- 以下のような流れ
-- factorial 3
--     = 3 * factorial 2
--     = 3 * (2 * factorial 1)
--     = 3 * (2 * (1 * factorial 0))
--     = 3 * (2 * (1 * 1))
--     = 6


-- 階乗の計算パターンマッチングバージョン
-- factorial :: Int -> Int
-- 引数が0のときは1を返す
-- factorial 0 = 1
-- 0でないときは再起処理
-- factorial x = x * factorial (x - 1)

-- main = print $ factorial 5  -- 出力: 120


-- 複数引数のパターンマッチング（パターンは上から順番に評価される）
-- power :: Int -> Double -> Double
-- 第一引数が0のときは1を返す
-- power 0 _ = 1.0
-- 0でないときは再帰処理（x * y^x を計算）
-- power x y = y * power (x - 1) y

-- main = print $ power 8 2.0  -- 出力: 256


-- 関数を引数や返り値に持つ関数のことを，高階関数（higher-order function）という
-- square x = x * x

-- 以下の,map関数は第一引数の関数を第2引数のリストの要素全てに適用する
-- idは引数をそのまま返す関数
-- succは整数の場合+1する関数
-- main :: IO ()
-- main = do print $ map id [1, 2, 3, 4, 5]      -- 出力: [1, 2, 3, 4, 5]
--           print $ map succ [1, 2, 3, 4, 5]    -- 出力: [2, 3, 4, 5, 6]
--           print $ map square [1, 2, 3, 4, 5]  -- 出力: [1, 4, 9, 16, 25]


-- 引数の関数 f を 2 回適用する関数 twice を定義
-- twice :: (a -> a) -> a -> a
-- twice f x = f (f x)

-- square :: Int -> Int
-- square x = x * x

-- main = do print $ twice id 2            -- 値: 2
--           print $ twice succ 2          -- 値: 4
--           print $ twice square 2        -- 値: 16
--           print $ twice twice square 2  -- 値: 65536

-- 関数式（関数を一行で書くやつ）
-- \を使うとワンライナーで書ける
-- add = \x y -> x + y
-- main = print $ map (\x -> x * x) [1, 2, 3, 4, 5]  -- 値: [1, 4, 9, 16, 25]


-- 部分適用
-- mult :: Int -> Int -> Int
-- mult x y = x * y

-- doubleはmultの第2引数を2に固定した関数
-- double 3 を実行するとmult 2 3が実行されたのと同じことが起きる
-- double :: Int -> Int
-- double = mult 2

-- 以下は同じ結果が得られる
-- main = print $ double 3  -- 出力: 6
-- main = print $ mult 2 3  -- 出力: 6


-- 自前で累乗の演算子を定義
-- (^^^) :: Double -> Int -> Double
-- _ ^^^ 0 = 1.0
-- x ^^^ y = x * (x ^^^ (y - 1))

-- main = print $ 2 ^^^ 10


-- 中置演算子は，括弧 ( …​ ) で囲むことにより，一般の関数と同じように扱える
-- main = print $ (+) 2 3   -- 値: 5
-- main = print $ (-) 2 3   -- 値: -1

-- 2引数関数は，バッククォート ` …​ ` で囲むことにより，中置形式で扱える
-- main = print $ 5 `div` 2   -- 値: 2
-- main = print $ 5 `mod` 2   -- 値: 1


-- 中置演算子の部分適用には，セクション（section）と呼ばれる記法が利用できる
-- (x +)  -- \y -> x + y に同じ
-- (+ y)  -- \x -> x + y に同じ
-- 以下はmap (\x -> 2 * x) [1, 2, 3, 4, 5]と同じ
-- main = print $ map (2 *) [1, 2, 3, 4, 5]  -- 値: [2, 4, 6, 8, 10]

-- 式 (- x) はセクションとは解釈されず，単項マイナス演算子の適用とみなされる


-- リストを扱う基本的な関数
-- main = print $ head [1, 2, 3]      -- 値: 1         （先頭要素を返す）
-- main = print $ tail [1, 2, 3]      -- 値: [2, 3]    （先頭要素を除いたリストを返す）
-- main = print $ [1] ++ [2, 3]       -- 値: [1, 2, 3] （リストを連結する）
-- main = print $ length [1, 2, 3]    -- 値: 3         （リストの長さを返す）
-- main = print $ map (2*) [1, 2, 3]  -- 値: [2, 4, 6] （各要素に関数を適用する）



-- 関数 foldr は，二項演算子を使って右からリストを畳み込む関数． 例えば，式 foldr (+) 0 [1, 2, 3] は，1 + (2 + (3 + 0)) と展開される．
-- main = print $ foldr (+) 0 [1, 2, 3]  -- 1 + (2 + (3 + 0)) =
-- main = print $ foldr (-) 0 [1, 2, 3]  -- 1 - (2 - (3 - 0)) = 2 左からやると-6


-- 数列表記
-- main = print $ [1..5]        -- 値: [1, 2, 3, 4, 5]
-- main = print $ [1, 3..10]    -- 値: [1, 3, 5, 7, 9]
-- main = print $ take 5 [1..]  -- 値: [1, 2, 3, 4, 5]

-- main = print $ [x      | x <- [1..10], odd x]         -- 値: [1, 3, 5, 7, 9]
-- main = print $ [(x, y) | x <- [1, 2], y <- [3, 4]]    -- 値: [(1,3), (1,4), (2,3), (2,4)]
-- main = print $ [(x, y) | x <- [1, 2, 3], let y = 4]   -- 値: [(1,4), (2,4), (3,4)]


-- クイックソートの実装
-- qsort [] = []
-- qsort (p:xs) = qsort smaller ++ [p] ++ qsort larger
--     where smaller = [x | x <- xs, x < p ]
--           larger  = [x | x <- xs, x >= p]

-- main = print $ qsort [3, 2, 4, 1, 5]  -- 出力: [1, 2, 3, 4, 5]


-- 図形を表すデータ型 Shape と，図形の面積を求める関数 area を定義
-- data Shape = Rect Double Double  -- 長方形
--            | Tri  Double Double  -- 三角形

-- area :: Shape -> Double
-- area(Rect x y)= x * y
-- area(Tri  x y)= x * y / 2

-- main = do print $ area(Rect 3 4) -- 出力: 12.0
--           print $ area(Tri  3 4) -- 出力: 6.0


-- 曜日を表すデータ型 DayOfWeek とそれを扱う関数を定義
-- data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun

-- 土日はtrueで他はfalse
-- holiday :: DayOfWeek -> Bool
-- holiday Sat = True
-- holiday Sun = True
-- holiday _   = False

-- main = do
--   print $ holiday Sun  -- 出力: True
--   print $ holiday Mon  -- 出力: False


-- name, age は フィールドラベル（field label）と呼ばれ，フィールドへのアクセサの役割を果たす
-- data Person = Person { name :: String, age :: Int }

-- 年齢を1増やす関数
-- inc :: Person -> Person
-- inc p = p { age = age p + 1 }

-- どっちでもいい
-- taro = Person { name = "Taro", age = 25 }
-- taro = Person "Taro" 25

-- main = do print $ name taro  -- 出力: "Taro"
--           print $ age taro   -- 出力: 25

-- main = print $ age $ inc taro   -- 出力: 26


-- "0123" のような数字列を表す型 DigitString を定義している
-- 関数 atoi は，数字列を整数に変換する関数．
-- 引数が数字列であるべきことを強調するために，String 型ではなく DigitString 型の値を引数に要求している．
-- newtype DigitString = DigitStr String

-- atoi :: DigitString -> Int
-- atoi(DigitStr xs)= read xs

-- main = print $ atoi(DigitStr "0123") -- 出力: 123


-- コロン : で始まる演算子を使うこともできる
-- 有理数を表す型 Ratio を定義
-- data Ratio = Integer :/ Integer

-- ratioToFloat :: Ratio -> Float
-- ratioToFloat(x :/ y)= fromIntegral x / fromIntegral y

-- main = do print $ ratioToFloat(3 :/ 2)  -- 出力: 1.5
--           print $ ratioToFloat(10 :/ 3) -- 出力: 3.33...


-- リストの要素の和を求める関数 sum
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- Int 以外の数値型のリストに sum を適用できない
-- sum :: [Int] -> Int

-- 数値以外のリストにも sum を適用できることになってしまう
-- sum :: [a] -> a

-- 型クラスを用いることで，数値のリストにのみ sum を適用できるようにする
-- sum :: Num a => [a] -> a


-- 1 次元空間上の点を表す Point 型を定義
-- data Point = Pt Double

-- instance Eq Point where
--   (Pt x) == (Pt x') = x == x'

-- instance Ord Point where
--   compare (Pt x) (Pt x')
--     | x == x'   = EQ
--     | x < x'    = LT
--     | otherwise = GT

-- main = do print $ (Pt 1) == (Pt 2)  -- 出力: False
--           print $ (Pt 1) >= (Pt 2)  -- 出力: False
--           print $ (Pt 1) <= (Pt 2)  -- 出力: True


-- Maybe を Container クラスのインスタンスとしたもの
-- class Container c where
--   cmap :: (a -> b) -> c a -> c b

-- instance Container Maybe where
--   -- cmap :: (a -> b) -> Maybe a -> Maybe b
--   cmap f Nothing  = Nothing
--   cmap f (Just x) = Just (f x)

-- main = do print $ cmap (2*) Nothing   -- 出力: Nothing
--           print $ cmap (2*) (Just 3)  -- 出力: Just 6


-- モジュール Data.List に定義されている関数 sort を利用する
-- import Data.List

-- main = do print $ sort [2, 1, 3]            -- 出力: [1, 2, 3]
--           print $ Data.List.sort [2, 1, 3]  -- 出力: [1, 2, 3]


-- 自作モジュールの利用
-- import Geometry

-- main = do print $ width (Rect 3 4)   -- 出力: 3.0
--           print $ height (Rect 3 4)  -- 出力: 4.0
--           print $ area (Tri 3 4)     -- 出力: 6.0
--           print $ area (Rect 3 4)    -- 出力: 12.0


-- I/O
-- main :: IO ()
-- main = putStrLn "hello, world"


-- 複数の処理
-- main :: IO（）
-- main = do
--     putStrLn "What's your name?"
--     name <- getLine
--     let greeting = "Hello, " ++ name ++ "!"
--     putStrLn greeting


-- テキストファイルの読み込みと表示
-- import System.IO

-- main :: IO()
-- main = do
--     handle <- openFile "hoge.txt" ReadMode
--     text <- hGetContents handle
--     putStr text
--     hClose handle



-- コマンドライン引数とプログラム名を表示する
-- import System.Environment

-- main :: IO()
-- main = do
--     -- 引数を取得
--     args <- getArgs
--     print args
--     -- ファイル名を取得
--     name <- getProgName
--     putStrLn name


-- モナド
-- x が 2 で割り切れない場合には失敗する
-- div2 :: Int -> Maybe Int
-- div2 x = if even x then Just (x `div` 2)
--                    else Nothing

-- -- x が 8 で割り切れない場合には失敗する
-- div8 :: Int -> Maybe Int
-- div8 x = return x >>= div2 >>= div2 >>= div2

-- main = do print $ div8 32  -- 出力: Just 4
--           print $ div8 50  -- 出力: Nothing



import Data.Char

main :: IO ()
main = do s <- getLine
          let t = map toUpper s
          if null s then return ()
                    else do { putStrLn t ; main }


