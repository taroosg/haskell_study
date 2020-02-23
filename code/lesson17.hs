import Data.List

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- 全部条件と満たすとき
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- どれか1つでも条件を満たすとき
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldl (||) False) . (map testFunc)

-- Semigroupでは常に同じ型を返す必要がある
-- なので除算はできない
instance Semigroup Integer where
  (<>) x y = x + y  -- <>という演算子を+として定義している

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Clear deriving (Show ,Eq)
instance Semigroup Color where
  -- Red <> Blueと記述するとPurpleを返す
  -- 単位元としてClearを追加
  (<>) Clear any = any
  (<>) any Clear = any
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  -- ↓aとbを入力してa==bのときはaを返す，の意味
  (<>) a b | a == b = a
            -- 全部の入力値がRed, Blue, Purpleのどれかだったとき，の意味
           | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
           | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown

instance Monoid Color where
  -- Clearが単位元
  mempty = Clear
  -- mappend関数を定義（Semigroupの演算を設定）
  mappend c1 c2 = (<>) c1 c2


  -- monoidは入力と出力が同じでないといけない
  -- 出力が文字列になるのでNG
  -- mconcat ['a','b','c','d','e']

data Events = Events [String]
data Probs = Probs [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

-- eventとprobsをいい感じの形（[event | prob \n]）に整形する
-- 単純に文字列と数値を入力していい感じにするだけ
showPair :: String -> Double -> String
showPair events probs = mconcat [events, "|", show probs, "\n"]

-- eventのリストと確率のリストをzipWithして全部つなげて（mconcat）表示
instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where pairs = zipWith showPair events probs

-- createPTable ["heads","tails"] [0.5,0.5]するとheads|0.5 \n tails|0.5が返る

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        -- [0.5, 0.5]を[[0.5, 0.5, 0.5], [0.5, 0.5, 0.5]]みたいな感じにする
        repeatedL1 = map (take nToAdd . repeat) l1
        -- [[0.5, 0.5, 0.5], [0.5, 0.5, 0.5]]を[0.5, 0.5, 0.5, 0.5, 0.5, 0.5]みたいにする
        newL1 = mconcat repeatedL1
        -- newL1が何桁でもいいようにl2を無限長にしておく
        cycledL2 = cycle l2

-- 2つのイベントをいい感じにくっつける関数（heads-tailsの形）
combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

-- 2つの確率をいい感じにくっつける関数（cartCombineのfuncに入れるといい感じになる）
combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)


instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1 -- PTableが空の場合
  -- PTable型はStringのイベントとDoubleの確率から定義される
  -- 2つの確率テーブルを<>で演算するといい感じに組み合わさったテーブルになって出力される
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

-- Monoidにmemptyとmappendを定義する
-- mappendには上で設定した<>を使用する
-- Monoidはmempty（単位元：加算なら0，乗算なら1など）とmappend（やりたい演算）を定義すれば良い
instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

-- コインの確率テーブルを定義
-- createPTable関数にイベントのリストと確率のリストを入力
-- 入力するときはEvents型とProbs型を明示する
coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

-- スピナーの確率テーブルを定義
-- 入力するときはEvents型とProbs型を明示する
spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])

-- 下記のようにするとcoinとspinnerを組み合わせた確率テーブルができる
-- coin <> spinner

-- 下記のようにするとcoinを3回投げたときの確率テーブルができる
-- mconcat [coin, coin, coin]

main = do
  print(mconcat [coin, coin, spinner])

-- import Data.List

-- myLast :: [a] -> a
-- myLast = head . reverse

-- myMin :: Ord a => [a] -> a
-- myMin = head . sort

-- myMax :: Ord a => [a] -> a
-- myMax = myLast . sort

-- myAny :: (a -> Bool) -> [a] -> Bool
-- myAny testFunc = (foldl (||) False) . (map testFunc)

-- instance Semigroup Integer where
--   (<>) x y = x + y  -- <>を+として定義

-- data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   -- これは結合律を満たさない
--   -- (<>) a b = if a == b
--   --   then a
--   --   else Brown
--   -- 結合的になるように書く
--   -- aとb療法が配列のどれかであるときはそれぞれの色になる
--   (<>) a b | a == b = a
--     | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
--     | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
--     | all (`elem` [Yellow, Red, Orange]) [a, b] = Orange
--     | otherwise = Brown

-- -- class Semigroup a => Monoid a where
-- --   identity :: a


-- -- 確率の計算
-- type Events = [String]
-- type Probs = [Double]

-- data PTable = PTable Events Probs

-- createPTable :: Events -> Probs -> PTable
-- createPTable events probs = PTable events normalizedProbs
--   where totalProbs = sum probs
--         normalizedProbs = map (\x -> x/totalProbs) probs

-- showPair :: String -> Double -> String
-- showPair event prob = mconcat [event, "|", show prob, "\n"]

-- instance Show PTable where
--   show (PTable events probs) = mconcat pairs
--     where pairs = zipWith showPair events probs

-- cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
-- cartCombine func l1 l2 = zipWith func newL1 cycledL2
--   where nToAdd = length l2
--         repeatedL1 = map (take nToAdd . repeat) l1
--         newL1 = mconcat repeatedL1
--         cycledL2 = cycle l2

-- combineEvents :: Events -> Events -> Events
-- combineEvents e1 e2 = cartCombine combiner e1 e2
--   where combiner = (\x y -> mconcat [x, "-", y])

-- combineProbs :: Probs -> Probs -> Probs
-- combineProbs p1 p2 = cartCombine (*) p1 p2

-- instance Semigroup PTable where
--   -- 2つの確率テーブルが存在する場合を想定しているが，はじめに一つしか存在しない場合を決めておく
--   (<>) ptable1 (PTable [] []) = ptable1
--   (<>) (PTable [] []) ptable2 = ptable2
--   -- 2つの確率テーブルが両方とも存在する場合
--   (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
--     where newEvents = combineEvents e1 e2
--           newProbs = combineProbs p1 p2

-- instance Monoid PTable where
--   -- 空である場合の要素の定義
--   mempty = PTable [] []
--   -- appendの処理
--   mappend = (<>)

-- coin :: PTable
-- coin = createPTable ["heads", "tails"] [0.5, 0.5]

-- spinner :: PTable
-- spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

-- main = do
--   print (mconcat [coin, coin, coin])