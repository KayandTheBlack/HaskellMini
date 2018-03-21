absValue :: Int -> Int

absValue n =
	if n < 0 then - n
	else n

power :: Int -> Int -> Int

power _ 0 = 1
power x p = x * power x (p-1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x  = not (hasDivisor 2)
	where hasDivisor d 
		| d*d > x = False
		| mod x d == 0 = True
		| otherwise = hasDivisor (d+1)


slowFib :: Int -> Int

slowFib 0 = 0
slowFib 1 = 1
slowFib n = (slowFib(n-1)) + (slowFib (n-2))

quickFib :: Int -> Int

quickFib n = x
	where (x,y) = quickFib2 n

-- quickFib = fst . quickFib2  -- lol
quickFib2 :: Int -> (Int,Int)
quickFib2 0 = (0,1)
quickFib2 n = (y, x+y)
	where (x,y) = quickFib2 (n-1)
