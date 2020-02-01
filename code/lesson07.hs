-- 最大公約数を求める関数
-- 再起では終わりの条件を決めるのが大事！！！
-- 終わりの条件を決める
-- 終わりの条件を満たしたらどうするかを決める
-- 終わり以外の条件を揃える
-- 終わり以外の条件でどうすれば終わりの条件に近づくのかを考える
myGCD a b = if remainder == 0
  -- 余りが0ならばbを返す
  then b
  -- そうでなければ，bを余りで割る処理をする
  else myGCD b remainder
  where remainder = mod a b

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  n -> "a bunch"

-- [1,2,3]が(1:[2,3])として認識される
myHead (x:xs) = x
myTail (_:xs) = xs

myTail2 [] = [1]
myTail2 (_:xs) = xs

-- パターンマッチングで最大公約数
-- 割るほうが0になったら終わり
myGCD2 a 0 = a
-- 0でない場合は割るほうを余りで割る
myGCD2 a b = myGCD2 b (mod a b)

main = do
  print(myGCD2 10 45)