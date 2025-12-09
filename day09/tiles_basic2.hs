import qualified Data.List as L

type Position = (Int, Int)
zeroPosition = (0,0) :: Position
type Rect = (Position, Position)
zeroRect = (zeroPosition, zeroPosition) :: Rect
type CandidateMax = (Int, Rect) 
zeroCandidate = (0, zeroRect) :: CandidateMax

data Line = Hori Int Int Int | Vert Int Int Int deriving(Show)
newLine :: Position -> Position -> Line
newLine (fx,fy) (tx,ty)
    | fx == tx = Vert fx (min fy ty) (max fy ty)
    | fy == ty = Hori fy (min fx tx) (max fx tx)

newRect :: Position -> Position -> Rect
newRect p1@(x1,y1) p2@(x2,y2)
    | x1 <= x2 && y1 <= y2 = (p1, p2) -- p1 is top left
    | x1 > x2 && y1 > y2 = (p2, p1) -- p1 is bottom right
    | x1 <= x2 && y1 > y2 = ((x1, y2), (x2, y1)) -- p1 is bottom left
    | x1 > x2 && y1 <= y2 = ((x2, y1), (x1, y2)) -- p1 is top right

area :: Rect -> Int
area ((x1,y1), (x2,y2)) = x*y
    where x = succ $ abs $ x1-x2
          y = succ $ abs $ y1-y2

contains :: Rect -> Position -> Bool
((x1,y1),(x2,y2)) `contains` (x, y)
    | inRangeExcl (x1,x2) x && inRangeExcl (y1,y2) y = True
    | otherwise = False

overlaps :: Rect -> Line -> Bool
r@(r1@(rx1, ry1), r2@(rx2, ry2)) `overlaps` Vert x y1 y2
    | r `contains` (x,y1) || r `contains` (x, y2) = True
    | inRangeIncl (rx1, rx2) x && inRangeIncl (y1, y2) ry1 && inRangeIncl (y1, y2) ry2 = True
    | otherwise = False
r@(r1@(rx1, ry1), r2@(rx2, ry2)) `overlaps` Hori y x1 x2
    | r `contains` (x1,y) || r `contains` (x2, y) = True
    | inRangeIncl (ry1, ry2) y && inRangeIncl (x1, x2) rx1 && inRangeIncl (x1, x2) rx2 = True
    | otherwise = False

inRangeIncl :: (Ord a) => (a,a) -> a -> Bool
inRangeIncl (x1,x2) x
    | x1 >= x2 = x <= x1 && x >= x2
    | x1 <  x2 = x >= x1 && x <= x2

inRangeExcl :: (Ord a) => (a,a) -> a -> Bool
inRangeExcl (x1,x2) x
    | x1 >= x2 = x < x1 && x > x2
    | x1 <  x2 = x > x1 && x < x2


rotation (x1, y1) (x2, y2)
    | x1 == x2 && y2 > y1 = 1
    | x1 == x2 && y2 < y1 = 3
    | y1 == y2 && x2 > x1 = 0
    | y1 == y2 && x2 < x1 = 2
    | otherwise = error "no direction for zero vector"

rotate :: Int -> Position -> Position
rotate n (x,y)
    | n `mod` 4 == 0 = (x,y)
    | ((n `mod` 4) + 4) `mod` 4 == 1 = (-y,  x)
    | ((n `mod` 4) + 4) `mod` 4 == 2 = (-x, -y)
    | ((n `mod` 4) + 4) `mod` 4 == 3 = ( y, -x)

rotToPlusX :: Position -> Position -> Position -> (Int, Position, Position, Position)
rotToPlusX a b c = let n = negate $ rotation a b
                    in (negate n, rotate n a, rotate n b, rotate n c)

calcBoundary :: [Position] -> [Line]
calcBoundary positions@(a:_:_:_:_) = concat $ map (\(a,b) -> [a,b]) $ aux a (drop 1 $ cycle positions)
    where aux :: Position -> [Position] -> [(Line, Line)]
          aux term (a:b:c:d:rest)
            | collinear a b c = aux (if (b == term) then a else term) (a:c:d:rest)
            | term == a = [aux' a b c]
            | otherwise = (aux' a b c) : aux term (b:c:d:rest)
          aux' :: Position -> Position -> Position -> (Line, Line)
          aux' a b c = let (rot, a'@(ax, ay), b'@(bx, by), c'@(cx, cy)) = rotToPlusX a b c
                           [e1, e2, e3] = if cy > ay
                                then map (rotate rot) $ [(ax+1,ay-1), (bx+1, by-1), (cx+1, cy-1)] -- turn right
                                else map (rotate rot) $ [(ax+1,ay-1), (bx-1, by-1), (cx-1, cy+1)] -- turn left
                        in (newLine e1 e2, newLine e2 e3)

-- "calc edges for " ++ (concat $ L.intersperse "->" $ map show $ [a, b, c])

collinear :: Position -> Position -> Position -> Bool
collinear (x1, y1) (x2, y2) (x3, y3)
    | x1 == x2 && x2 == x3 = True
    | y1 == y2 && y2 == y3 = True
    | otherwise = False



solve :: [Position] -> [(Int, Rect)]
solve positions = let edges = calcBoundary positions
                   in scanl (\curmax x -> max curmax
                                (foldl (\candidate y -> let rect = newRect x y
                                                         in if any (rect `overlaps`) edges
                                                            then candidate
                                                            else max (area rect, rect) candidate
                                       ) zeroCandidate positions)
                            ) zeroCandidate positions

parse :: String -> [Position]
parse = map (\p -> read $ '(':p++")"). lines

bruteforce_max :: [Position] -> CandidateMax
bruteforce_max [a,b] = let rect = newRect a b in (area rect, rect)
bruteforce_max (x:rest) = max (bruteforce_max rest)
                              (foldl (\acc y -> max acc
                                                ((area $ newRect x y), (newRect x y))
                                     ) zeroCandidate rest
                              )

main' file = do
    contents <- readFile file
    let parsed = parse contents
        (ans1, rect1) = bruteforce_max parsed
    putStrLn $ "Task 1: " ++ (show ans1)

