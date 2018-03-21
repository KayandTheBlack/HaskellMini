data Queue a = Queue [a] [a] 
	deriving (Show)

create:: Queue a
create = Queue [] []

push:: a -> Queue a -> Queue a
push x (Queue a b) = Queue a (x:b)

pop:: Queue a -> Queue a
pop (Queue [] b) = Queue (drop 1 (reverse b)) []
pop (Queue (x:a) b) = Queue a b

top::  Queue a -> a
top (Queue [] b) = last b
top (Queue (x:_) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a) 
	where
		Queue (a:as) b == Queue (x:xs) y =
			x==a && Queue as b == Queue xs y
		Queue [] b == Queue x y = b == y ++ reverse x
		Queue a b == Queue [] y = y == b ++ reverse a
