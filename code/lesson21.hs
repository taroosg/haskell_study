import System.Random


mystery1 :: Int -> Int -> Int
mystery1 val1 val2 = (val1 + val2 + val3)^2
  where val3 = 3

mystery2 :: Int -> Int -> IO Int
mystery2 val1 val2 = do
  putStrLn "Enter a number"
  val3Input <- getLine
  let val3 = read val3Input
  return ((val1 + val2 + val3)^2)

-- mystery1では動作するがmystery2ではコンパイルエラー
-- mystery2はユーザ入力という不純な操作があるため純粋関数とは組み合わせられない
safeValue = (mystery1 2 4) + (mystery1 5 6)

-- サイコロの処理

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  dieRpll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRpll)

-- main = do
--   print (safeValue)