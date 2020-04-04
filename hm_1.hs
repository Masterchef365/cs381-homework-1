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

--
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag (x:xs) b = checkbag x b && subbag xs b
--subbag (x:xs) b = checkbag x b || subbag xs b

test_bag = [(5,1),(7,3),(2,1),(3,2),(8,1)]

main = do
    print "Insertion:"
    print $ ins 99 test_bag 
    print $ ins 3 test_bag 
    print $ ins 1 test_bag 
    print $ ins 7 $ ins 8 test_bag 

    print "Deletion:"
    print $ del 99 test_bag 
    print $ del 3 test_bag 
    print $ del 1 test_bag 
    print $ del 2 test_bag 

    print "Creation:"
    print $ bag [2, 3, 3, 5, 7, 7, 7, 8]
    print $ bag [7,3,8,7,3,2,7,5]

    print "Check:"
    print $ checkbag (5, 3) test_bag 
    print $ checkbag (3, 1) test_bag 
    print $ checkbag (99, 1) test_bag 

    print "Sub:"
    print $ subbag [(5,1),(7,5),(2,1),(3,2),(8,1)] test_bag 
    print $ subbag [(5,1),(7,1),(2,1),(3,1),(8,1)] test_bag 
    print $ subbag [(5,1),(7,3),(2,1),(8,1)] test_bag 
    print $ subbag test_bag [(5,1),(7,3),(2,1),(8,1)]
    print $ subbag test_bag test_bag 
