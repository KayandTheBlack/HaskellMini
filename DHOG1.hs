myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x: myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f x
	| c x = x
	| otherwise = myUntil c f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f a = [f x | x<-a]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter c a = [x|x<-a, c x]

myAll :: (a -> Bool) -> [a] -> Bool
myAll c a = foldr (\x acc -> c x && acc) True a


myAny :: (a -> Bool) -> [a] -> Bool
myAny c a = foldr (\x acc -> c x || acc) False a

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b= [f x y | (x,y) <- myZip a b]

