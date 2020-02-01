class Describable a where
  describe :: a -> String

-- 不等号の型クラスを表示．「Ordを実装している同じ型を2つとりBoolを返す」の意味
-- (>) :: Ord a => a -> a -> Bool

-- Showの派生クラスである旨を定義
-- Ordは記述順で判断する
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

main = do
  print(Chocolate > Vanilla)