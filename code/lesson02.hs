simple x = x

incriment x = x + 1

calcChange01 owed given = if given - owed > 0
  then given - owed
  else 0

calcChange02 owed given = if change > 0
  then change
  else 0
  where
    change = given - owed

doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

inc n = n + 1

double n = n * 2

square n = n ^ 2

oddEven n = if even n
  then n - 2
  else 3 * n + 1

main = do
  print (oddEven 1000)