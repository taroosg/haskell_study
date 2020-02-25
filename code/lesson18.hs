import Data.Char
-- 既存のMap関数と競合を回避するためqualified文を使用する
-- モジュールの型と関数を使用する際には`Map`をつける必要がある
import qualified Data.Map as Map

data Box a = Box a deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box val) = Box (func val)

n = 6 :: Int

word = "box"
-- Box word :: Box [Char]

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- タプルは型が異なっていても良いが，下記では同じ型でないといけないように定義している
data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple v1 v2 v3) = Triple (func v1) (func v2) (func v3)

-- 例1
-- Triple Double Double Doubleの型
type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- 例2
-- Triple String String Stringの型
type FullName = Triple String
aParson :: FullName
aParson = Triple "Howard" "Phillips" "Lovecraft"

-- 例3
-- Triple Char Char Charの型
type Initial = Triple Char
initials :: Initial
initials = Triple 'H' 'P' 'L'

-- 一度定義すればいいようにしておく
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

-- List型にしてみる
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

-- Listに対するmap的な関数を定義
-- 関数fを全ての要素に適用
-- mapは型の変換ができるが，transform関数は型の変更ができない
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- リスト型を独自に定義してみる
-- Listの型が再帰的になっている点に注目
data List a = Empty | Cons a (List a) deriving Show

-- 組み込み例1
builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

-- 独自定義例1
-- 右辺が再帰的な感じ
ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- 組み込み例2
builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

-- 独自定義例2
ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- map関数の実装
-- Emptyは乗算できないのでパターンマッチで定義する必要がある
ourMap :: (a -> b) -> List a -> List b
ourMap func Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

-- タプルの定義
-- 文具の在庫を想定して定義
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

-- 在庫まとめ
itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]


-- 臓器の型定義
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

-- 数が増えるとつらい
-- pairs = [(2, Heart),(7, Heart), ...]

-- zipで2つのリストをまとめる
organPairs :: [(Int, Organ)]
organPairs = zip ids organs

-- カタログをつくる
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

-- 全てのパーツから成るリストを定義
allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

-- パーツの数を数える
organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where countOrgan = (\organ -> (length . filter (==organ)) values)

organInventry :: Map.Map Organ Int
organInventry = Map.fromList (zip allOrgans organCounts)

main = do
  print (Map.lookup 7 organCatalog)