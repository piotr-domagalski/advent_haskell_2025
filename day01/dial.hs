import System.Environment

newtype Dial = Dial { getPosition :: Int } deriving(Show, Eq, Ord)

newDial :: Int -> Dial
newDial i = Dial { getPosition = i }

turn :: Int -> Dial -> Dial
turn i (Dial {getPosition=pos}) = Dial {getPosition=newpos}
    where newpos = (pos + i) `mod` 100

data CountZeros = CountZeros Int Dial deriving(Show, Eq)

turn' :: Int -> CountZeros -> CountZeros
turn' i (CountZeros c d)
    | getPosition newd == 0 = CountZeros (c+1) newd
    | otherwise = CountZeros c newd
    where newd = turn i d

turn'' :: Int -> CountZeros -> CountZeros
turn'' i (CountZeros c d)
    | getPosition newd == 0 = CountZeros (c+fullturns+1) newd
    | otherwise = CountZeros (c+fullturns+crossed) newd
    where newd = turn i d
          fullturns = abs(i `quot` 100)
          maybecrossed = if 0 `compare` i /= d `compare` newd then 1 else 0
          crossed = if getPosition d == 0 then 0 else maybecrossed

main = do
    file:_ <- getArgs
    main' file

main' :: String -> IO ()
main' file = do
   contents <- readFile file
   let turns = parse contents
       ans1 = solve1 turns
       ans1' = solve1' turns
       ans2 = solve2 turns
   putStrLn $ "stopped on zero: " ++ show ans1
   putStrLn $ "stopped on zero 2: " ++ show ans1'
   putStrLn $ "crossed zero: " ++ show ans2

parse :: String -> [Int]
parse = map (read . \(x:xs) -> (if x == 'L' then '-' else ' '):xs) . lines

-- solve :: [Int] -> Int
solve1 = length
      . filter (==0)
      . scanl (\acc v -> (acc+v) `mod` 100) 50

solve1' :: [Int] -> CountZeros
solve1' = foldl (flip turn') (CountZeros 0 (newDial 50))

solve2 :: [Int] -> CountZeros
solve2 = foldl (flip turn'') (CountZeros 0 (newDial 50))
