
type Position = (Int, Int)

area :: Position -> Position -> Int
area (x1,y1) (x2,y2) = x*y
    where x = succ $ abs $ x1-x2
          y = succ $ abs $ y1-y2

parse :: String -> [Position]
parse = map (\p -> read $ '(':p++")"). lines

bruteforce_max :: [Position] -> (Int, Position, Position)
bruteforce_max [a,b] = (area a b, a, b)
bruteforce_max (x:rest) = max (bruteforce_max rest)
                              (foldl (\acc y -> max acc ((area x y), x, y)) (0, (0,0), (0,0)) rest)

main' file = do 
    contents <- readFile file
    let parsed = parse contents
        (ans1, p11, p12) = bruteforce_max parsed
    putStrLn $ "Task 1: " ++ (show ans1)

