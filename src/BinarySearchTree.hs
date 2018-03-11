module BinarySearchTree where

import qualified Data.List
-- You might want to use some externals. Better use qualified import
-- so there won't be any clash
-- for example instead of "sort" (from Data.List) use "Data.List.sort"

-- !! DO NOT CHANGE BSTree data type and type signatures of functions

-- | Binary search tree as described at wikipedia:
--  https://en.wikipedia.org/wiki/Binary_search_tree
data BSTree a = Node a (BSTree a) (BSTree a)
              | Nil
              deriving (Show, Read, Eq)

value :: BSTree a -> a
value Nil = error "Nil does not have a value"
value (Node x _ _) = x

left :: BSTree a -> BSTree a
left Nil = Nil
left (Node _ l _) = l

right :: BSTree a -> BSTree a
right Nil = Nil
right (Node _ _ r) = r

-- | Check whether is @BSTree@ valid (i.e., does not violate any rule)
-- TODO: implement validity check
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x <= head (xs) && isSorted xs

isValid :: Ord a => BSTree a -> Bool
isValid = isSorted . toList

-- | Check whether is @BSTree@ is leaf
-- TODO: implement leaf check
isLeaf :: Ord a => BSTree a -> Bool
isLeaf (Node _ Nil Nil) = True
isLeaf _ = False

-- | Count all nodes in @BSTree@
-- TODO: implement counting all nodes of the tree
size :: BSTree a -> Integer
size tree = toInteger (length (toList tree))

-- | Height of @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out height of the tree
height :: BSTree a -> Integer
height Nil = 0
height (Node _ left right) = if height (left) > height (right) then height (left) + 1 else height (right) + 1

-- | Minimal height in the @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out minimal depth of the tree
minHeight :: BSTree a -> Integer
minHeight Nil = 0
minHeight (Node _ left right) = if minHeight (left) < minHeight (right) then minHeight (left) + 1 else minHeight (right) + 1

-- | Check if given element is in the @BSTree@
-- TODO: implement finding out if element is in the tree
contains :: Ord a => BSTree a -> a -> Bool
contains Nil _ = False
contains (Node x left right) v
    | v == x = True
    | v < x = contains left v
    | v > x = contains right v

-- | Create new tree with given element inserted
-- TODO: implement insertion to the tree
insert :: Ord a => BSTree a -> a -> BSTree a
insert Nil v = Node v Nil Nil
insert (Node x left right ) v 
    | v == x = Node x left right
    | v < x = Node x (insert left v) right
    | v > x = Node x left (insert right v)

-- | Create new tree with given element deleted (min element in the right subtree strategy)
-- TODO: implement deletion from the tree
delete :: Ord a => BSTree a -> a -> BSTree a
delete Nil _ = Nil
delete (Node x left right) v
    | v == x = deleteRoot (Node x left right)
    | v < x = Node x (delete left v) right
    | v > x = Node x left (delete right v)

deleteRoot :: Ord a => BSTree a -> BSTree a
deleteRoot (Node x Nil right) = right
deleteRoot (Node x left Nil) = left
deleteRoot (Node x left right) = (Node (swapStrat right) left (delete right (swapStrat right)))

swapStrat :: Ord a => BSTree a -> a
swapStrat (Node x Nil _) = x
swapStrat (Node _ left _) = swapStrat left

-- | Convert @BSTree@ to list (will be in ascending order if tree is valid)
-- TODO: implement conversion from tree to list
toList :: BSTree a -> [a]
toList Nil = []
toList (Node x left right) = toList left ++ [x] ++ toList right

-- | Build new @BSTree@ from arbitrary list with use of median (left if even)
-- TODO: implement conversion from list to tree, use median (hint: sort)
fromList :: Ord a => [a] -> BSTree a
fromList [] = Nil
fromList l = createTree (fixSortDupl l)

fixSortDupl :: Ord a => [a] -> [a]
fixSortDupl = Data.List.map Data.List.head . Data.List.group . Data.List.sort

createTree :: Ord a => [a] -> BSTree a
createTree [] = Nil
createTree l = Node medianValue (createTree lleft) (createTree lright)
        where
            medianIndex = div ((length l) - 1) 2
            medianValue = l !! medianIndex
            lleft = take medianIndex l
            lright = drop (medianIndex + 1) l
