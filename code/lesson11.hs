x::Int
x = 2

y::Integer
y = 2

letter::Char
letter = 'a'

interestRate::Double
interestRate = 0.375

isFun::Bool
isFun = True

values::[Int]
values = [1,2,3]

testScores::[Double]
testScores = [0.99,0.7,0.8]

letters::[Char]
letters = ['a','b','c']
-- "abc"と同じ

aPet::[Char]
aPet = "cat"

anotherPet::String
anotherPet = "dog"

ageAndHeight::(Int,Int)
ageAndHeight = (34,74)

firstlastMiddle::(String,String,Char)
firstlastMiddle = ("Oscar","Grouch",'D')

streetAddress::(Int,String)
streetAddress = (123,"Happy St")

double::Int -> Int
double n = n * 2

-- Intは半分に割れないので引数のIntをDoubleにキャストする必要がある
half::Int -> Double
half n = (fromIntegral n) / 2

halve::Integer -> Integer
halve n = div n 2

-- 変換後の型を明示してreadを用いて文字列からキャストする
-- どちらでも良い
-- anotherNumber::Int
anotherNumber = read "6"::Double

printDouble::Int -> String
printDouble n = show (n * 2)

-- 第1引数の型，第2引数の型，第3引数の型，戻り値の型（最後が戻り値）
makeAddress :: Int -> String -> String -> (Int,String,String)
makeAddress number street town = (number,street,town)

-- Haskellの関数は引数を一つしか受け取れない
-- ラムダ関数で書き換えた場合
makeAddressLambda = (\number ->
                (\street ->
                  (\town -> (number,street,town))))

-- 部分適用になるのでどれでも呼び出せる
-- ((makeAddressLambda 123) "Happy st") "Haskell Town"
-- ((makeAddress 123) "Happy st") "Haskell Town"
-- makeAddress 123 "Happy st" "Haskell Town"

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
  then f n
  else n

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

makeTriple :: a -> b -> c-> (a,b,c)
makeTriple x y z = (x,y,z)

myMap :: (a -> b) -> [a] -> [b]
myMap f list = map f list

myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

main = do
  print (myTail [1,2,3,4])
