eql :: [Int] -> [Int] -> Bool
eql l1 l2 = (length l1 == length l2) && and (zipWith (==) l1 l2)

prod :: [Int] -> Int
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens = prod . filter even

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = foldl (+) 0 (zipWith (*) x y)

