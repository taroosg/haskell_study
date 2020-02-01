type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int
type PatientName = (String,String)
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

-- パターンマッチングでミドルネームの有無を分ける（型コンストラクタの差異で判断）
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

-- 名前のサンプル
name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patient age height = name ++ " " ++ ageHeight
  where name = lastName patient ++ ", " ++ firstName patient
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- 新しい型の作成
data Sex = Male | Female

-- 性別を1文字にして取得する関数
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
-- 型コンストラクタを使用（型名と同じでなくてもいいが同じほうがわかりやすい）
data BloodType = BloodType ABOType RhType
-- 患者データに必要な名前，性別，年齢，身長，体重，血液型を格納する型を定義
-- data Patient = Patient Name Sex Int Int Int BloodType
-- 上の型定義をレコード構文を使用して記述
data Patient = Patient {
  name :: Name,
  sex :: Sex,
  age :: Int,
  height :: Int,
  weight :: Int,
  bloodType :: BloodType
}

-- サンプル患者
johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- サンプル患者（レコード構文）
-- height jackieSmithなどで取れる
jackieSmith :: Patient
jackieSmith = Patient {
  name = Name "Jackie" "Smith",
  sex = Female,
  age = 43,
  height = 62,
  weight = 115,
  bloodType = BloodType O Neg
}
jackieSmithupdated = jackieSmith {
  age = 44
}

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
-- 下のpatient1BTなどが同じ形になるので引数に渡せる．他には(BloodType B Neg)などでもOK
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
-- OからほかはOK
canDonateTo (BloodType O _) _ = True
-- 他からABはOK
canDonateTo _ (BloodType AB _) = True
-- AからA
canDonateTo (BloodType A _) (BloodType A _) = True
-- BからB
canDonateTo (BloodType B _) (BloodType B _) = True
-- 他の組み合わせはNg
canDonateTo _ _ = False

-- 患者を引数にとり，輸血可否を判定する関数
donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

patient1BT :: BloodType
-- 右辺のBloodTypeは型コンストラクタ
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

patientSummary :: Patient -> String
patientSummary patient = "***\n" ++
  "Patient Name: " ++ showName (name patient) ++ "\n" ++
  "Sex: " ++ showSex (sex patient) ++ "\n" ++
  "Age: " ++ show (age patient) ++ "\n" ++
  "Height: " ++ show (height patient) ++ " in.\n" ++
  "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
  "Bloot Types: " ++ showBloodType (bloodType patient) ++ "\n"

-- 名前表示
-- print (showName (name jackieSmith))

main = do
  print (patientSummary jackieSmith)