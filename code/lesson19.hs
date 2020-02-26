import qualified Data.Map as Map
import Data.Maybe
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- 全ての引き出しをリストで定義
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog

-- getDrawerContentsに引き出しの総数と現在定義しているカタログを入力すると引き出しの臓器有無を全て出力する
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- [Maybe Organ]から値を取り出す必要がない
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

-- NothingはFalse，ほかはTrueにする関数を定義
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

-- availableOrgans（Nothingがたくさんあるやつ）をisSomethingでfilterしたのでNothingでないやつのみ残る
justTheOrgans :: [Maybe Organ]
-- justTheOrgans = filter isSomething availableOrgans
-- `Data.Maybe`をimportすると`isJust`が使える
-- 型が`Just _`のときにTrueになるので，これを使ってfilterできる
-- ただし，全てのデータの前に`Just`がつく
justTheOrgans = filter isJust availableOrgans

-- justを除去するために関数を定義
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

-- `Data.List`をimportすると`intercalate`が使用できる
-- `intercalate`はinsert的な意味
-- Listを指定した文字列で区切って文字列にする．join的な動き
cleanList :: String
cleanList = intercalate ", " organList

-- 引数で`Maybe Int`を受け取り，Nothingなら0，それ以外なら値を返す
numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just n) = n

-- マッドサイエンティストのルール
-- 脳はバット（Vat）に入れる
-- 心臓は保冷機（Cooler）に入れる
-- 脾臓と腎臓はバッグ（Bag）に入れる

-- 場所のルール
-- バットと保冷機は研究室（Lab）に置く
-- 袋はキッチン（Kitchen）に置く

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- 臓器を入力すると場所のタプルを出力する関数
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

-- 場所のタプルを入力すると臓器がどこにあるかの説明文を出力する関数
report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

-- Nothingかどうかで処理を分ける関数
processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

-- リクエストを処理する関数
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

-- エラー発生時の処理が`processRequest`で行っているが，本来は`report`で行いたい

-- Map.lookupの戻り値はMaybe Organ
-- idが存在しない場合はNothingを返す
-- Nothingの型はMaybe Organなのでnullにはならない
-- Maybe型とOrgan型はShowをサポートしているので`show availableOrgans`で表示ができる
main = do
  print (processRequest 13 organCatalog)