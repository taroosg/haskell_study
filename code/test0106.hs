-- xのy乗を計算する
power x y = case y of
              0 -> 1
              _ -> x * power x (y - 1)

main = do
  print (power 2 123)
