import Data.List

-- instanceを使用する場合はderivingの代わりに使う（重複不可）
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

-- SixSidedDieに対してShowメソッドを定義する
instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- (==)を定義すると(/=)も自動的に定義される
-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

-- instance Ord SixSidedDie where
--   compare S6 S6 = EQ
--   compare S6 _ = GT
--   compare _ S6 = LT
--   compare S5 S5 = EQ
--   compare S5 _ = GT
--   compare _ S5 = LT
--   compare S4 S4 = EQ
--   compare S4 _ = GT
--   compare _ S4 = LT
--   compare S3 S3 = EQ
--   compare S3 _ = GT
--   compare _ S3 = LT
--   compare S2 S2 = EQ
--   compare S2 _ = GT
--   compare _ S2 = LT
--   compare S1 S1 = EQ
  -- 以下はすでに上で定義した中に含まれるので定義してはいけない
  -- compare S1 _ = GT
  -- compare _ S1 = LT

-- ↓のdataはnewtypeでも可
data Name = Name (String,String) deriving (Show, Eq)
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [
  Name ("Email","Cioran"),
  Name ("Eugene","Thacker"),
  Name ("Friedrich","Nietzche")
  ]

data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Show, Eq, Enum)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (mod n 5)

main = do
  print(sort names)