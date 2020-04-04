import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub

type Bag a = [(a,Int)]

ins :: Eq a => a -> Bag a -> Bag a
ins v [] = [(v, 1)]
ins v (x@(a, b):xs)
    | fst x == v = (a, succ b) : xs 
    | otherwise = x : ins v xs

bag = [(5,1),(7,3),(2,1),(3,2),(8,1)]

main = print $ ins 99 bag 
