-- 適当に定義
-- data AutherName = AutherName String String
-- data AutherName = AutherName {
--   firstName :: String,
--   lastName :: String
-- }

-- 適当に定義
-- data Book = Book AutherName String String Int Double
-- data Book = Book {
--   auther :: AutherName,
--   isbn :: String,
--   year :: Int,
--   price :: String
-- }

-- 直和型でミドルネームの有無を考慮したName型を定義
-- 直和型は既存の型2つで構成してもいいし3つで構成してもいい感じのやつ
type FirstName = String
type LastName = String
type MiddleName = String

-- Name型に新しいパターンが必要になったら下に追加するだけで良い
data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

-- 以下3つはName型に依存することになる
data Creator = AutherCreator Auther | ArtistCreator Artist
data Auther = Auther Name
data Artist = Parson Name | Band String

hpLovecraft :: Creator
hpLovecraft = AutherCreator (Auther (TwoInitialsLast 'H' 'P' "Livecraft"))

data Book = Book {
  auther :: Creator,
  isbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double
}

-- 同じパラメータ名をつけられない点に注意（対策在り）
data VinyRecord = VinyRecord {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double
}

-- あとから追加が容易であるのが直和型のメリット
data CollectibleToy = CollectibleToy {
  name :: String,
  description :: String,
  toyPrice :: Double
}

data StoreItem = BookItem Book
  | RecordItem VinyRecord
  | ToyItem CollectibleToy

-- それぞれの価格を表す関数をパターンマッチングで実装
price :: StoreItem ->  Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

main = do
  print("hoge")