
type Position = (Int, Int)
zeroPosition :: Position
zeroPosition = (0, 0)
type Rect = (Position,Position)
zeroRect :: Rect
zeroRect = (zeroPosition, zeroPosition)
type CandidateMax = (Int, Rect)
zeroCandidate :: CandidateMax
zeroCandidate = (0, zeroRect)

           --    x   y1  y2         y   x1  x2
data Line = Vert Int Int Int | Hori Int Int Int

area :: Position -> Position -> Int
area (x1,y1) (x2,y2) = x*y
    where x = succ $ abs $ x1-x2
          y = succ $ abs $ y1-y2

contains :: Rect -> Position -> Bool
((x1,y1),(x2,y2)) `contains` (x, y)
    | inRangeExcl (x1,x2) x && inRangeExcl (y1,y2) y = True
    | otherwise = False

overlaps_bad :: Rect -> Rect -> Bool
l@(l1@(lx1, ly1), l2@(lx2, ly2)) `overlaps_bad` r@(r1@(rx1, ry1), r2@(rx2, ry2))
    | l `contains` r1 || l `contains` r2 = True
    | r `contains` l1 || r `contains` l2 = True
    | l `contains` (lx1, ly2) || l `contains` (lx2, ly1) = True
    | inRangeExcl (lx1, lx2) rx1 && inRangeExcl (lx1, lx2) rx2 &&
      not (inRangeExcl (ly1, ly2) ry1) && not (inRangeExcl (ly1, ly2) ry2) &&
      ((ry1 < ly1 && ry2 > ly1) || (ry1 < ly1 && ry2 > ly1)) = True
    | otherwise = False -- TODO: incomplete

overlaps :: Rect -> Line -> Bool
r@(r1@(rx1, ry1), r2@(rx2, ry2)) `overlaps` Vert x y1 y2
    | r `contains` (x,y1) || r `contains` (x, y2) = True
    | inRangeIncl (rx1, rx2) x && inRangeIncl (y1, y2) ry1 && inRangeIncl (y1, y2) ry2 = True
    | otherwise = False
r@(r1@(rx1, ry1), r2@(rx2, ry2)) `overlaps` Hori y x1 x2
    | r `contains` (x1,y) || r `contains` (x2, y) = True
    | inRangeIncl (ry1, ry2) y && inRangeIncl (x1, x2) rx1 && inRangeIncl (x1, x2) rx2 = True
    | otherwise = False

inRangeExcl :: (Ord a) => (a,a) -> a -> Bool
inRangeExcl (x1,x2) x
    | x1 >= x2 = x < x1 && x > x2
    | x1 <  x2 = x > x1 && x < x2

inRangeIncl :: (Ord a) => (a,a) -> a -> Bool
inRangeIncl (x1,x2) x
    | x1 >= x2 = x <= x1 && x >= x2
    | x1 <  x2 = x >= x1 && x <= x2

toLine :: Position -> Position -> Line
toLine (x1,y1) (x2,y2)
    | x1 == x2 = Hori x1 y1 y2
    | y1 == y2 = Vert y1 x1 x2
    | otherwise = error $ "Line cannot be diagonal - got: " ++ (show (x1,y1)) ++ " " ++ (show (x2,y2))

toOuterEdge :: Position -> Position -> Line
toOuterEdge from@(xf,yf) to@(xt, yt)
    | xf == xt && yf < yt = toLine (xf+1, yf+1) (xt+1, yt-1) -- right
    | xf == xt && yf > yt = toLine (xf-1, yf-1) (xt-1, yt+1) -- left
    | yf == yt && xf < xt = toLine (xf+1, yf-1) (xt-1, yt-1) -- top
    | yf == yt && xf > xt = toLine (xf-1, yf+1) (xt+1, yf+1) -- bottom
    | xf /= xt || yf /= yt = error "aaa"
    | otherwise = error "bbb"


toOuterEdges :: [Position] -> [Line]
toOuterEdges pos@(a:_:_:_:_) = aux a $ drop 1 $ cycle pos
    where aux :: Position -> [Position] -> [Line]
          aux fst (a:b:c:pos)
            | fst == a = 
toOuterEdges _ = error "cannot construct outer edges with <4 points"

parse :: String -> [Position]
parse = map (\p -> read $ '(':p++")"). lines

bruteforce_max :: [Position] -> CandidateMax
bruteforce_max [a,b] = (area a b, (a, b))
bruteforce_max (x:rest) = max (bruteforce_max rest)
                              (foldl (\acc y -> max acc ((area x y), (x, y))) zeroCandidate rest)

bruteforce_max2 :: [Position] -> CandidateMax
bruteforce_max2 positions = foldl (\curmax x -> max curmax $
                                    foldl (\potmax y -> if any ((x,y) `contains`) positions
                                                        then zeroCandidate
                                                        else max potmax ((area x y), (x, y))
                                          ) zeroCandidate positions
                                  ) zeroCandidate positions
-- for each corner
-- find which kind of corner it is: tl, tr, t, bl, br, b, l, r
--      in a list? t = [tl, tr], l = [tl, bl] ...
-- filter all positions according to kind (eg a is bl -> b.x >= a.x && b.y >= a.y)

main' file = do 
    contents <- readFile file
    let parsed = parse contents
        (ans1, rect1) = bruteforce_max parsed
        (ans2, rect2) = bruteforce_max2 parsed
    putStrLn $ "Task 1: " ++ (show ans1)
    putStrLn $ "Task 2: " ++ (show (ans2, rect2))

