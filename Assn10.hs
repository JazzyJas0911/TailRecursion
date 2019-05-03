fact :: Int -> Int
fact n =
   if n == 1 then 1
   else n * (fact (n - 1))

fac :: Int -> Int
fac 1 = 1
fac n = n * (fac (n-1))


tailFac :: Int -> Int
tailFac n = loop n 1
     where loop x y
             | x == 1 = y
             | otherwise = loop (x - 1) (x * y)


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


tailFib :: Int -> Int
tailFib n = loop n 0 1
    where loop x y z
           | x == 0 = y
           | x == 1 = z
           | otherwise = loop (x - 1) z (y + z)


main = do
    print (fact 10)
    print (fac 10)
    print (tailFac 10)
    print (fib 10)
    print (tailFib 0)
    print (tailFib 1)
    print (tailFib 2)
    print (tailFib 3)
    print (tailFib 4)
    print (tailFib 5)
    print (tailFib 6)
    print (tailFib 10)
