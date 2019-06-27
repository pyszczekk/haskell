
data List a = Empty | Elem{ head::a, tail::List a}
	deriving(Show,Eq,Ord)

list = Elem 1 (Elem 2 (Elem 3 (Elem 4 Empty)))

map' :: (a->b) -> List a -> List b
map' _ Empty = Empty
map' f (Elem h t) = Elem (f h) (map' f t)

foldr' ::  (a -> b -> b) -> b -> List a -> b
foldr' f z Empty = z
foldr' f z (Elem h t) =  f h (foldr' f z t) 

foldl' ::  (b -> a -> b) -> b -> List a -> b
foldl' f z Empty = z
foldl' f z (Elem h t) =   foldl' f (f z h) t

zipWith' :: (a -> b -> c) -> List a -> List b -> List c 
zipWith' _ Empty _ = Empty  
zipWith' _ _ Empty = Empty 
zipWith' f (Elem x xs) (Elem y ys) = Elem (f x y) (zipWith' f xs ys)  

infixr 5 +++
(+++)::List a -> List a -> List a
(+++) (Elem h t) Empty = Elem h t
(+++) Empty (Elem h t) = Elem h t
(+++) Empty Empty = Empty
(+++) (Elem h t) (Elem h2 t2)=Elem h (t +++ (Elem h2 t2))

dodaj :: List a -> List a -> List a 
dodaj (Elem h t) Empty = Elem h t
dodaj Empty (Elem h t) = Elem h t
dodaj Empty Empty = Empty
dodaj (Elem h t) (Elem h2 t2) = Elem h (dodaj t (Elem h2 t2))

fromNormalList:: [a] -> List a
fromNormalList [] = Empty
fromNormalList (x:xs) = Elem x (fromNormalList xs)

toNormalList :: List a -> [a]
toNormalList Empty = []
toNormalList (Elem h t) = h: toNormalList t

instance Functor List where
	fmap _ Empty = Empty
	fmap f (Elem h t) = Elem (f h) (fmap f t)


instance Applicative List where
	pure a = Elem a Empty
	Empty <*> _ = Empty
	(Elem h t) <*> sth =dodaj (fmap h sth) (t <*> sth)

