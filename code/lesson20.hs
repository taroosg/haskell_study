import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [
  (1, 200.1),
  (2, 199.5),
  (3, 199.4),
  (4, 198.9),
  (5, 199.0),
  (6, 200.2),
  (9, 200.3),
  (10, 201.2),
  (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 = [
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 203.5),
  (15, 204.9),
  (16, 207.1),
  (18, 210.5),
  (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 = [
  (10, 201.2),
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 201.3),
  (17, 210.5),
  (24, 215.1),
  (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 = [
  (26, 219.8),
  (27, 220.5),
  (28, 223.8),
  (29, 222.8),
  (30, 223.8),
  (31, 221.7),
  (32, 222.3),
  (33, 220.8),
  (34, 219.4),
  (35, 220.1),
  (36, 220.6)
  ]

-- データを入れるためのTS型（データが欠損している場合もあるのでMaybe型を使用）
data TS a = TS [Int] [Maybe a]

-- TS型を作成するためのインターフェースを定義する関数（時間と値を入れるとTS型のデータにして出力）
createTS times values = TS completeTimes extendedValues
  where completeTimes = [minimum times .. maximum times]
        timeValueMap = Map.fromList (zip times values)
        extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

-- ファイルのデータをTS型に変換しやすくする関数
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where splitPairs = (unzip tvPairs)
        times = fst splitPairs
        values = snd splitPairs

-- 時間と値のペアを表示するための関数
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

-- 上記関数を使用してすべてのファイルをTS型に変換
ts1 :: TS Double
ts1 = fileToTS file1
ts2 :: TS Double
ts2 = fileToTS file2
ts3 :: TS Double
ts3 = fileToTS file3
ts4 :: TS Double
ts4 = fileToTS file4

-- キーバリューを(k, Maybe v)に変換
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
-- 時間をすべて組み合わせる
  where bothTimes = mconcat [t1, t2]
        completeTimes = [(minimum t1) .. (maximum t2)]
        -- ts1の値を空のMapに挿入する
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        -- ts2の値を上の業で作成したMapに挿入する．自動的に重複する値が上書きされる
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        -- updatedMapにlookupを適用するとMaybe型のリストが得られる
        combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

-- mconcat :: Monoid a => [a] -> a
instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

-- ファイルの中身をリストに入れて全部まとめる関数
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- 平均を求める関数
-- 数値系の値の配列を平均して結果を返す
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
-- 全部のvaluesがNothingだった場合．．．
meanTS (TS times values) = if all (==Nothing) values
  then Nothing
  else Just avg
    -- Justかどうかのテスト
    where justVals = filter isJust values
          -- 全部にjustValsを実行してパスしたものだけのリストを作成
          cleanVals = map (\(Just x) -> x) justVals
          -- 全部Justのリストの平均値
          avg = mean cleanVals

-- 比較関数の型定義
type CompareFunc a = (a -> a -> a)
-- TS型を比較する関数の型定義
type TSCompareFunc a = ((Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a))

-- 比較関数を入力すると入力した関数で比較した結果を返す関数
-- makeTSCompare min (3, Just 100) (4, Just 10)のように使える
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  -- whereの中でパターンマッチング
  -- 両方Nothingの場合
  where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        -- 片方Nothingの場合
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        -- 両方valが存在するとき
        -- 両方を比較して結果によってどちらかを返す
        newFunc (i1, Just val1) (i2, Just val2) = if (func val1 val2) == val1
          then (i1, Just val1)
          else (i2, Just val2)

-- TS型データに比較関数を適用する関数
-- 比較関数とTSデータを入力すると比較関数に引っかかったタプルを返す
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (==Nothing) values
  then Nothing
  else Just best
    where pairs = zip times values
          -- 順番に(o, Nothing)と比較する
          best = foldl (makeTSCompare func) (0, Nothing) pairs

-- compareTSにminを入力
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

-- compareTSにmaxを入力
maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

main = do
  print (maxTS tsAll)