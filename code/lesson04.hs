import Data.List

ifEvenInc01 n = if even n
  then n + 1
  else n

ifEvenDouble01 n = if even n
  then n * 2
  else n

ifEvenSquare01 n = if even n
  then n^2
  else n

ifEven myFunction x = if even x
  then myFunction x
  else x

inc n = n + 1
double n = n * 2
square n = n^2
cube n = n^3

ifEvenInc02 n = ifEven inc n
ifEvenDouble02 n = ifEven double n
ifEvenSquare02 n = ifEven square n
ifEvenCube n = ifEven (\x -> x^3) n

auther = ("Taro", "Osg")

names = [("Taro", "Osg"),
  ("Yuri", "Osg"),
  ("Mari", "Oki"),
  ("C", "K")]

compairLastNames01 name1 name2 = if lastName1 > lastName2
  then GT
  else if lastName1 < lastName2
    then LT
    else EQ
  where lastName1 = snd name1
        lastName2 = snd name2

compairLastNames02 name1 name2 = if lastName1 > lastName2
  then GT
  else if lastName1 < lastName2
    then LT
    else if firstName1 > firstName2
      then GT
      else if firstName1 < firstName2
        then LT
        else EQ
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2

addressLetter01 name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

sfOffice name = if lastName < "L"
  then nameText ++ " - " ++ "PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - " ++ "PO Box 1010 - San Francisco, CA, 94109"
  where nameText = (fst name) ++ " " ++ (snd name)
        lastName = (snd name)

nyOffice name = nameText ++ " : " ++ "PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " : " ++ "PO Box 456 - Reno, NV, 89523"
  where nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter02 name location = locationFunction name
  where locationFunction = getLocationFunction location

-- 引数の順番を入れ替える関数
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

-- 強引に入れ替えたパターン
-- addressLetterV2 location name= addressLetter02 name location

addressLetterV2 = flipBinaryArgs addressLetter02

addressLetterNY = addressLetterV2 "ny"

main = do
  print (addressLetterNY ("Taro", "Osg"))