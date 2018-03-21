ones :: [Integer]
ones = repeat 1

nats :: [Integer] 
nats = natsAux 0
	where natsAux n = n:natsAux(n+1)

ints :: [Integer] 
ints = 0:intsAux 1
	where intsAux n = n:(-n:intsAux(n+1))

triangulars :: [Integer]
triangulars = triangularsAux 0 1
	where triangularsAux n add = n:triangularsAux (n+add) (add+1)

factorials :: [Integer]
factorials = factorialsAux 1 1
	where factorialsAux n x = n:factorialsAux (n*x) (x+1)

fibs :: [Integer] 
fibs = fibsAux 0 1
	where fibsAux n n' = n:fibsAux n' (n+n')

primes :: [Integer] 
primes = primesAux (iterate (+1) 2)
	where primesAux (x:xs) = x:primesAux (filter (\y -> mod y x /=0) xs)

hammings :: [Integer] 
hammings = 1:(merge (map (*2) hammings) (merge (map (*3) hammings) (map (*5) hammings)))

merge (x:l1) (y:l2)
	| x==y = x:merge l1 l2
	| x<y  = x:merge l1 (y:l2)
	| otherwise = y:merge (x:l1) l2
	
lookNsay :: [Integer] 
lookNsay = iterate doAndSay 1
	where
		doAndSay n = toInteg (transform (toList n))
		transform [] = []
		transform (x:xs) = (toInteger (length l1)) + 1 : x : transform l2
			where (l1,l2) = span (==x) xs
		toList 0 = []
		toList n = (toList (div n 10)) ++ [mod n 10]
		toInteg [] = 0
		toInteg (x:xs) = x* 10^length xs + toInteg xs

tartaglia :: [[Integer]]
tartaglia = iterate (\row -> zipWith (+) (0:row) (row++[0])) [1]
