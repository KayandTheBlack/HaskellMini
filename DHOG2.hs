countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = foldl (\acc x -> if f x then acc+1 else acc) 0 l

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = map (\f -> map f xs) fs 

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> map (\f -> f x) fs) xs

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl c f i l = foldl (\acc x -> if c x then f acc x else acc) i l

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f l x = l1 ++ (x:l2)
	where
		(l1,l2) = span (`f` x) l

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f l = foldl (\acc x -> insert f acc x) [] l
