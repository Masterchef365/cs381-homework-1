import Data.List (nub,sort)
norm :: Ord a => [a] -> [a]
norm = sort . nub

type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

-- Return a list of the nodes in the graph
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm $ fst x : snd x : nodes xs

-- Find the successors for a node
suc :: Node -> Graph -> [Node]
suc _ [] = []
suc n ((v, e):xs)
    | v == n = e : suc n xs
    | otherwise = suc n xs

-- Detach an edge from a graph
detach :: Node -> Graph -> Graph
detach  _ [] = []
detach n (x@(v, e):xs)
    | v == n || e == n = detach n xs
    | otherwise = x : detach n xs

cyc :: Int -> Graph
cyc n = (n, 1) : [(x, succ x) | x <- [1,2..pred n]]

main = do
    print "Nodes"
    print $ nodes g == [1,2,3,4]
    print $ nodes h == [1,2,3,4]
    print $ nodes [(1, 0)]

    print "Suc"
    print $ suc 2 g == [3, 4]
    print $ suc 2 h == [1]

    print "Detach"
    print $ detach 3 g == [(1,2),(2,4)]
    print $ detach 2 h == [(1,3),(4,4)]

    print "Cyc"
    print $ cyc 4 == [(4,1),(1,2),(2,3),(3,4)]
