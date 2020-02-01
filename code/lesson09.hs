import Data.Char

-- myFilter test [] = []::[Char]
-- myFilter test (x:xs) = if test x
--   then x:myFilter test xs
--   else myFilter test xs

-- myRemove test [] = []::[Char]
-- myRemove test (x:xs) = if test x
--   then myRemove test xs
--   else x:myRemove test xs

myProduct [] = 1
myProduct (x:xs) = 1 * x * myProduct xs
myProduct2 xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myElem val xs = (length (filter (\x -> x == val) xs)) > 0

-- toLowerするにはimport Data.Charが必要
isPalindrome text = lowerText == reverse lowerText
  where lowerText = map toLower (filter (\x -> x /= ' ') text)

-- A man a plan a canal Panama

main = do
  print (isPalindrome "A man a plan a canal Panama")
