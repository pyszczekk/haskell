
data Tree a = Leaf 
			| Node{value :: a,
			 left :: Tree a,
			 right::Tree a }

tree = Node 1 Leaf Leaf
tree2 = Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 19 (Node 16 Leaf Leaf) (Node 41 Leaf Leaf))
tree3 = Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 19 (Node 16 Leaf Leaf) (Node 41 Leaf Leaf))
tree4 = Node 10 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 19 (Node 16 Leaf Leaf) (Node 41 Leaf Leaf))
instance Show a => Show (Tree a) where
	show Leaf = "nil"
	show (Node v left right)= "{value: "++ show v ++" left: "++show left ++" right: " ++ show right ++ "}"

instance Eq a => Eq (Tree a) where
	(==) Leaf Leaf = True
	(==) (Node _ _ _) Leaf = False
	(==) Leaf (Node _ _ _) = False
	(==) (Node v l r) (Node v2 l2 r2) = (v==v2) && (l==l2) && (r==r2)

isEmpty :: Tree a -> Bool -- Sprawdza czy drzewo jest puste
isEmpty Leaf = True -- Jak puste zwróć True
isEmpty _ = False -- Jak nie to False

insert::(Ord a, Eq a)=> a-> Tree a -> Tree a
insert a Leaf = Node a Leaf Leaf 
insert a (Node v l r ) 
	|	(a < v )= Node v (insert a l) r
	| otherwise = Node v l (insert a r)


search :: (Ord a, Eq a )=>a -> Tree a -> Bool
search a Leaf = False
search a (Node v l r)
	| a == v = True
	| a < v = search a l
	| a > v = search a r


tmap :: (a->b) -> Tree a -> Tree b 
tmap _ Leaf = Leaf    --dowolna funkcja i puste Tree zwróci puste Tree
tmap f (Node val left right) = (Node (f val) (tmap f left) (tmap f right) )

instance Functor Tree where
	fmap _ Leaf = Leaf
	fmap f (Node val left right) = (Node (f val) (fmap f left) (fmap f right) )
	

instance Applicative Tree where
	pure a = Node a Leaf Leaf
	Leaf <*> _ = Leaf
	(Node a l r) <*> (Node a2 l2 r2) = (Node (a a2) ((Node a l r) <*> l2) ((Node a l r) <*> r2))
