myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
	| x > m = x
	| otherwise = m
	where m = myMaximum xs

average :: [Int] -> Float
average [] = 0
average x = (fromIntegral (mySum x)) / (fromIntegral (myLength x))
	where 
		mySum [] = 0
		mySum (x:xs) = x + (mySum xs)

buildPalindrome :: [Int] -> [Int]
{-buildPalindrome x = pal
	where
		pal = reversal x ++ x
		reversal :: [Int] -> [Int]
		reversal [] = []
		reversal (xi:xs) = reversal xs ++ [xi]
-}
-- reversal is super underoptimal! do double paramater function reversal x [] that reverts the list, returns second list when first is emtpy. (stack voiding) better if we do list 2 x!
buildPalindrome x = reversal x x
reversal [] y = y 
reversal (x:xs) y = reversal xs (x:y)


remove :: [Int] -> [Int] -> [Int]
remove x [] = x
remove x (y:ys) = remove (elremove x y) ys
	where 
		elremove (x:xs) y
			| x==y = elremove xs y
			| otherwise = [x] ++ elremove xs y
		elremove [] y = []

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
	| mod x 2 == 0 = (odds, x:evens)
	| otherwise = (x:odds, evens)
	where
		(odds,evens) = oddsNevens xs

primeDivisors :: Int -> [Int]
primeDivisors x = primeAux x 2
	where
		primeAux 1 _ = []
		primeAux x y
			| mod x y == 0 = (y:(primeAux (totdiv x y) (y+1)))
			| otherwise = primeAux x (y+1)
		totdiv x y
			| mod x y == 0 = totdiv (div x y) y
			| otherwise = x



