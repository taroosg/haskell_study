ifEven myFunction x = if even x
  then myFunction x
  else x

inc n = n + 1
double n = n * 2
square n = n^2
cube n = n^3

-- 最初に定義したパターン
-- ifEvenInc02 n = ifEven inc n
-- ifEvenDouble02 n = ifEven double n
-- ifEvenSquare02 n = ifEven square n

-- 部分適用を使ったパターン
ifEvenInc02 = ifEven inc
ifEvenDouble02 = ifEven double

-- 関数を入力して生成した関数を出力する関数
getIfEven f = (\x -> ifEven f x)

getIfXEven x = (\f-> ifEven f x)

-- 汎用的に使う順に引数を設定する
genRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

-- hostを入力すると，入力したhostに固定したurlビルド関数を返す関数
genHostRequestBuilder host = (\apiKey resource id -> genRequestUrl host apiKey resource id)

-- exampleを入力してhostを固定した関数．残りのパラメータを渡すとurlができる
exampleUrlBuilder = genHostRequestBuilder "https://example.com"

-- apiKeyとhostと決める関数を入力すると，入力値を固定したビルド関数を返す関数
genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

-- exampleに固定する関数とapiKeyを固定した関数．残り2つを渡すとurlができる
myExampleUrlBulder = genApiRequestBuilder exampleUrlBuilder "hasKell"
-- print(myExampleUrlBulder "hoge" "3214")

add4 a b c d = a + b + c + d

addXto3 x = (\b c d -> add4 x b c d)
add3 = addXto3 5

addXYto2 x y = (\c d -> add4 x y c d)
add2 = addXYto2 1 5

genIdBuilder = genRequestUrl "https://example.com" "hasKell" "book"

-- xから2をマイナスする関数を定義
subtract x = (-) x 2

-- flipで引数順を「2 x」として，2で部分適用した関数を定義
subtract2 = flip (-) 2

-- 関数と引数を受け取り，引数の入力を待つ関数を定義
binaryPatsialApplication binaryFunc arg = (\x -> binaryFunc arg x)

-- なにか引数を渡すと4から引数を引いた値を出力する関数
takeFromFour = binaryPatsialApplication (-) 4

main = do
  print(takeFromFour 100)

