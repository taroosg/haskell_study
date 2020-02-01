import Data.List

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldl (||) False) . (map testFunc)

instance Semigroup Integer where
  (<>) x y = x + y  -- <>を+として定義

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  -- これは結合律を満たさない
  -- (<>) a b = if a == b
  --   then a
  --   else Brown
  -- 結合的になるように書く
  -- aとb療法が配列のどれかであるときはそれぞれの色になる
  (<>) a b | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
    | all (`elem` [Yellow, Red, Orange]) [a, b] = Orange
    | otherwise = Brown

main = do
  print (Red <> (Yellow <> Orange))