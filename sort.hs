insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x:xs) y
	| x > y = y:(x:xs)
	| otherwise = x:(insert xs y)
isort :: [Int] -> [Int]
isort l = isortAux [] l
	where 
		isortAux l [] = l
		isortAux l (x:xs) = isortAux lp xs
			where lp = insert l x

remove :: [Int] -> Int -> [Int]
remove (x:xs) y
	| x==y = xs
	| otherwise = x:(remove xs y)
ssort :: [Int] -> [Int]
ssort l = ssortAux l []
	where 
		ssortAux [] l = l
		ssortAux l1 l2 = ssortAux (remove l1 x) (x:l2)
			where x = maximum l1

merge :: [Int] -> [Int] -> [Int]
merge [] l1 = l1
merge l1 [] = l1
merge (x:l1) (y:l2)
	| x > y = y:(merge (x:l1) l2)
	| otherwise = x:(merge l1 (y:l2))
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge s1 s2
	where 
		s1 = msort (take n l)
		s2 = msort (drop n l)
		n = (length l) `div` 2

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:l) = (qsort (filter (< x) l)) ++ [x] ++ (qsort (filter (>=x) l))

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:l) = (genQsort (filter (< x) l)) ++ [x] ++ (genQsort (filter (>=x) l))


