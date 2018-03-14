module Main(main) where
import           Test.HUnit

data TreeSet a = Nub | Node (TreeSet a) a (TreeSet a) deriving(Show)

add :: (Ord a) => a -> TreeSet a -> TreeSet a
add x Nub = Node Nub x Nub
add x tree@(Node left y right) | x < y     = Node (add x left) y right
                               | x > y     = Node left y (add x right)
                               | otherwise = tree

addall :: (Ord a) => [a] -> TreeSet a -> TreeSet a
addall as tree = foldl (flip add) tree as

treesetOf :: (Ord a) => [a] -> TreeSet a
treesetOf = foldl (flip add) Nub

inorder :: TreeSet a -> [a]
inorder Nub                 = []
inorder (Node left y right) = inorder left ++ y : inorder right

preorder :: TreeSet a -> [a]
preorder Nub                 = []
preorder (Node left y right) = y : preorder left ++ preorder right

postorder :: TreeSet a -> [a]
postorder Nub                 = []
postorder (Node left y right) = postorder left ++ postorder right ++ [y]

contains :: (Ord a) => a -> TreeSet a -> Bool
contains _ Nub = False
contains x (Node left y right) | x == y    = True
                               | x < y     = contains x left
                               | otherwise = contains x right

instance (Eq a) => Eq (TreeSet a) where
    tree1 == tree2 = inorder tree1 == inorder tree2

main :: IO Counts
main = runTestTT $ TestList [
            TestCase (assertEqual "Building singleton treeset" (show (Node Nub (1 :: Int) Nub)) (show (add (1 :: Int) Nub))),
            TestCase (assertEqual "Inorder traversal of treeset" [1::Int] $ inorder $ add (1::Int) Nub),
            TestCase (assertEqual "Inorder traversal of treeset" "abc" $ inorder $ addall "abc" Nub),
            TestCase (assertEqual "Inorder traversal of treeset" "abc" $ inorder $ addall "cba" Nub),
            TestCase (assertEqual "Inorder traversal of treeset" "abc" $ inorder $ addall "bca" Nub),
            TestCase (assertEqual "Treeset of abc should contain c" True $ contains 'c' $ addall "abc" Nub),
            TestCase (assertEqual "Treeset of abc should not contain d" False $ contains 'd' $ addall "abc" Nub),
            TestCase (assertEqual "treesetOf and addall _ Nub shuld be equivalent" (addall "abc" Nub) $ treesetOf "abc"),
            TestCase (assertEqual "inorder traversal of treeset abcdefg" ['a'..'g'] $ inorder $ treesetOf "bdfcgae"),
            TestCase (assertEqual "postorder traversal of treeset abcdefg" "acegfdb" $ postorder $ treesetOf "bdfcgae"),
            TestCase (assertEqual "preorder traversal of treeset abcdefg" "badcfeg" $ preorder $ treesetOf "bdfcgae")
        ]
