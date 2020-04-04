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

-- Calculate the minimum x coordinate of a shape
minX :: Shape -> Number
minX (Circle (x, y) r) = x - r
minX (Rect (x, y) w h) = x
minX (Pt (x, y)) = x

-- Add two points together
addPt :: Point -> Point -> Point
addPt (ax, ay) (bx, by) = (ax + bx, ay + by)

-- Move a shape by the coordinate
move :: Shape -> Point -> Shape
move (Circle a r) b = Circle (addPt a b) r
move (Rect a w h) b = Rect (addPt a b) w h
move (Pt a) b = Pt (addPt a b)

--- Set an elements minX to zero
zeroMinX :: Shape -> Shape
zeroMinX (Circle (_, y) r) = Circle (r, y) r
zeroMinX (Rect (_, y) w h) = Rect (0, y) w h
zeroMinX (Pt (_, y)) = Pt (0, y)

-- Align all figures so that their X minXs are the same  
alignLeft :: Figure -> Figure
alignLeft f = map zeroMinX f

f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

main = do
    print "Width"
    print $ map width f == [0,6,7]

    print "BBox"
    print $ map bbox f == [((4,4),(4,4)),((2,2),(8,8)),((3,3),(10,5))]

    print "MinX"
    print $ map minX f == [4,2,3]

    print "Move"
    print $ map (\l -> move l (1, -2)) f -- == [Pt (5,2), Circle (6,3) 3, Rect (4,1) 7 2]

    print "AlignLeft"
    print $ map minX (alignLeft f) == [0, 0, 0]
