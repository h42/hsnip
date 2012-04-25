data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a  = Node a (treeInsert x left) right
	| x > a  = Node a left (treeInsert x right)

fromList :: [Int] -> Tree Int
fromList xs = foldl (flip treeInsert) EmptyTree xs

tmap :: (a -> b) -> Tree a -> Tree b
tmap f EmptyTree  = EmptyTree
tmap f (Node x l r)  = Node (f x) (tmap f l) (tmap f r)

instance Functor Tree where
    fmap = tmap
