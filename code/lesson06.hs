
simple x = x

longlist = [1 .. ]

stillLongLost = simple longlist

-- 回文チェッカー
isPalindrome word = word == reverse word

-- 含むかどうかチェッカー
isInclude char word = elem char word

-- 含まれているかどうかで処理を分ける
respond phrase = if '!' `elem` phrase
  then "wow!"
  else "foo..."

takeList n aList = reverse (take n(reverse aList))

testList = [1,2,3,4,5,6,7,8,9,10]

-- 要素の削除（元のリストは変更されない）
hogeList n  = drop n testList

-- n個のxの配列
ones n x = take n (cycle [x])

-- nチームにわける関数
assignToGroups n aList = zip groups aList
  where groups = cycle [1 .. n]

-- 開始位置，終了位置，リストを受け取り間にあるリストを返す
subseq start end aList = drop start (take end aList)

inFirstHalf n aList = n `elem` aListFirst
  where aListFirst = take ((div) (length aList) 2) aList

main = do
  print(inFirstHalf 'o' "fheiwoafnp")