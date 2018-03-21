flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

myLength :: String -> Int
myLength = \x -> sum (map (const 1) x)

myReverse :: [Int] -> [Int]
myReverse = foldl (flip (:)) []

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map count l
	where
		count l = foldl (\acc y -> if x==y then acc + 1 else acc) 0 l

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') (dropWhile (==' ') s)

