data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node a t1 t2) = 1 + size t1 + size t2

height :: Tree a -> Int
height Empty = 0
height (Node a t1 t2) = 1 + max (height t1) (height t2)
	where 
		max x y
			| x>y = x
			| otherwise = y

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a t1 t2) (Node b u1 u2) = a==b && equal t1 u1 && equal t2 u2

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a t1 t2) (Node b u1 u2) = a==b && ((equal t1 u1 && equal t2 u2) || (equal t1 u2 && equal t2 u1))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a t1 t2) = a:((preOrder t1) ++ (preOrder t2))

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a t1 t2) = (postOrder t1) ++ (postOrder t2) ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a t1 t2) = (inOrder t1) ++ a:(inOrder t2)

breadthFirst :: Tree a -> [a]
breadthFirst x = breadthFirstAux [x]
	where
		breadthFirstAux [] = []
		breadthFirstAux (Empty:ts) = breadthFirstAux ts
		breadthFirstAux ((Node a t1 t2):ts) = a:(breadthFirstAux (ts ++ t1:[t2]))

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:xs) (y:ys) = Node x (build x1 y1) (build x2 y2)
	where 
		(y1,y2') = span (/= x) (y:ys)
		y2 = drop 1 y2'
		(x1,x2) = splitAt (length y1) xs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty t2 = t2
overlap _ t1 Empty = t1
overlap f (Node a t1 t2) (Node b u1 u2) = Node (f a b) (overlap f t1 u1) (overlap f t2 u2) 
 



