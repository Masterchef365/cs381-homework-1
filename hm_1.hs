import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub

type Bag a = [(a,Int)]

-- Insertion
ins :: Eq a => a -> Bag a -> Bag a
ins v [] = [(v, 1)]
ins v (x@(a, b):xs)
    | fst x == v = (a, succ b) : xs 
    | otherwise = x : ins v xs

-- Deletion
del :: Eq a => a -> Bag a -> Bag a
del v [] = []
del v (x@(a, b):xs)
    | a == v = if b > 1 then (a, pred b) : xs else xs 
    | otherwise = x : del v xs

-- Creation
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x $ bag xs

-- Check if a bag contains at least n of an element
checkbag :: Eq a => (a, Int) -> Bag a -> Bool
checkbag _ [] = False
checkbag v@(v_a, v_i) ((x_a, x_i):xs)
    | x_a == v_a && v_i <= x_i = True 
    | otherwise = checkbag v xs

-- Check if the first bag is a sub-bag of the second bag
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag (x:xs) b = checkbag x b && subbag xs b

inter :: Eq a => (a, Int) -> Bag a -> [(a, Int)]
inter _ [] = []
inter v@(v_a, v_i) ((x_a, x_i):xs)
    | x_a == v_a = [(v_a, min v_i x_i)]
    | otherwise = inter v xs

-- Intersection 
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] _ = []
isbag _ [] = []
isbag b (x:xs) = inter x b ++ isbag b xs

-- Length
size :: Bag a -> Int
size [] = 0
size ((_,i):xs) = i + size xs

test_bag = [(5,1),(7,3),(2,1),(3,2),(8,1)]

main = do
    print "Insertion:"
    print $ ins 99 test_bag == [(5,1),(7,3),(2,1),(3,2),(8,1),(99,1)]
    print $ ins 3 test_bag == [(5,1),(7,3),(2,1),(3,3),(8,1)]
    print $ (ins 7 $ ins 8 test_bag) == [(5,1),(7,4),(2,1),(3,2),(8,2)]

    print "Deletion:"
    print $ del 99 test_bag == test_bag
    print $ del 3 test_bag == [(5,1),(7,3),(2,1),(3,1),(8,1)]
    print $ del 2 test_bag == [(5,1),(7,3),(3,2),(8,1)]

    print "Creation:"
    print $ bag [2,3,3,5,7,7,7,8] == [(8,1),(7,3),(5,1),(3,2),(2,1)]
    print $ bag [7,3,8,7,3,2,7,5] == [(5,1),(7,3),(2,1),(3,2),(8,1)]

    print "Check:"
    print $ checkbag (5,3) test_bag == False
    print $ checkbag (3,1) test_bag == True
    print $ checkbag (99,1) test_bag == False

    print "Sub:"
    print $ subbag [(5,1),(7,5),(2,1),(3,2),(8,1)] test_bag == False
    print $ subbag [(5,1),(7,1),(2,1),(3,1),(8,1)] test_bag == True
    print $ subbag [(5,1),(7,3),(2,1),(8,1)] test_bag == True
    print $ subbag test_bag [(5,1),(7,3),(2,1),(8,1)] == False
    print $ subbag test_bag test_bag == True

    print "IsBag"
    print $ isbag [(5,2),(7,3),(2,1),(8,1)] [(5,1),(99,1)] == [(5,1)]
    print $ isbag [(1, 1)] [] == []
    print $ isbag [] [(1, 1)] == []

    print "Size"
    print $ size [] == 0
    print $ size [(1, 1)] == 1
    print $ size [(1, 8)] == 8
    print $ size [(1, 3),(2,7)] == 10
    print $ size test_bag == 8
