sumSquareOrSquareSum01 x y = if sumSquare > squareSum
  then sumSquare
  else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x+y)^2

sumSquareOrSquareSum02 x y = (\sumSquare squareSum ->
  if sumSquare > squareSum
    then sumSquare
    else squareSum) (x^2 + y^2) ((x+y)^2)

doubleDouble01 x = dubs*2
  where dubs = x * 2

doubleDouble02 x = (\dubs -> dubs * 2) (x * 2)

overwrite01 x = let x = 2
  in
    let x = 3
    in
      let x = 4
      in
        x

overwrite02 x =
  (\x ->
    (\x ->
      (\x -> x) 4
    ) 3
  ) 2

x = 4
add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y ->
  (\x-> y + x) 1) 2

counter01 x = let x = x + 1
  in
    let x = x + 1
    in
      x

counter02 x = (\x -> ((\x -> x + 1) x) + 1) x

main = do
  print(counter02 10)