-- この内容はlesson10
cup flOz = \message -> message flOz
aCup = cup 6
coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = if ozDiff >= 0
  then cup ozDiff
  else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

afterASip = drink coffeeCup 1
afterTwoSip = drink afterASip 1

afterManySips = foldl drink coffeeCup [1,1,1,1,1]

-- ロボットの実装
robot (name,attack,hp) = \message -> message (name,attack,hp)

killerRobot = robot ("Kill3r",25,200)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

-- getter
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- setter
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

-- 既存のオブジェクトのコピーに変更を加えることで新しいオブジェクトを生成する
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

-- 数値はそのまま表示できないのでshowを使う必要あり
printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

-- ダメージを与える処理
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))
afterHit = damage killerRobot 90

-- 戦闘の処理（fight 攻撃側 防御側の順番）
fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
          then getAttack aRobot
          else 0

-- 敵ロボットの定義
gentleGiant = robot ("Mr. Friendly",10,300)

-- 1ターン目
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot

-- 2ターン目（1ターン目の結果を使用）
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1

-- 3ターン目（2ターン目の結果を使用）
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- 対戦用の2つのロボットを定義
fastRobot = robot ("speedy",15,40)
slowRobot = robot ("slowpoke",20,30)

-- 1ターン目
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot

-- 2ターン目
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1

-- 2ターン目
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

main = do
  print(printRobot fastRobotRound3)
