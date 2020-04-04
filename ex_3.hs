type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
    | Circle Point Length -- (x, y), r
    | Rect Point Length Length -- (x, y), w, h
    deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

-- Calculate the width of a shape
width :: Shape -> Length
width (Circle _ r) = 2 * r
width (Rect _ w _) = w
width (Pt _) = 0

-- Calculate the bounding box of a shape
bbox :: Shape -> BBox
bbox (Circle (x, y) r) = ((x - r, y - r), (x + r, y + r))
bbox (Rect (x, y) w h) = ((x, y), (x + w, y + h))
bbox (Pt (x, y)) = ((x, y), (x, y))


f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

main = do
    print "Width"
    print $ map width f == [0,6,7]

    print "BBox"
    print $ map bbox f == [((4,4),(4,4)),((2,2),(8,8)),((3,3),(10,5))]
