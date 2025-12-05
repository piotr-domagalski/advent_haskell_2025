import System.Environment

dialNumberCount = 100
dialInitialPosition = 50

type Dial = Int
newDial = dialInitialPosition

turn :: Int -> Dial -> Dial
turn i pos = (pos + i) `mod` dialNumberCount

type ZeroCounter = (Int, Dial)
newZeroCounter = (0, newDial)

turn' :: Int -> ZeroCounter -> ZeroCounter
turn' i (c, d)
    | newd == 0 = ((c+1), newd)
    | otherwise = (c, newd)
    where newd = turn i d


{-
turn'' :: Int -> ZeroCounter -> ZeroCounter
turn'' i (c, d) = ((c+fullturns+crossed), newd)
    where newd = turn i d
          fullturns = abs(i `quot` dialNumberCount)
          crossed = if 0 `compare` i /= d `compare` newd then 1 else 0
-}

turn'' :: Int -> ZeroCounter -> ZeroCounter
turn'' i (c, d)
    | newd == 0 = ((c+fullturns+1), newd)
    | otherwise = ((c+fullturns+crossed), newd)
    where newd = turn i d
          fullturns = abs(i `quot` dialNumberCount)
          maybecrossed = if 0 `compare` i /= d `compare` newd then 1 else 0
          crossed = if d == 0 then 0 else maybecrossed

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
   putStrLn $ "stopped (scanl): " ++ show ans1 ++
              "    stopped (counter): " ++ show (fst ans1') ++
              "    crossed : " ++ show (fst ans2)

runfiles :: [String] -> IO ()
runfiles [] = do
    return ()
runfiles (x:xs) = do
    putStrLn x
    main' x
    runfiles xs

runtests = runfiles [ "test_cross_once_left.txt"
                    , "test_cross_once_right.txt"
                    , "test_reach_once_left.txt"
                    , "test_reach_once_right.txt"
                    ]

parse :: String -> [Int]
parse = map (read . \(x:xs) -> (if x == 'L' then '-' else ' '):xs) . lines

-- solve :: [Int] -> Int
solve1 = length
      . filter (==0)
      . scanl (\acc v -> (acc+v) `mod` dialNumberCount) dialInitialPosition

solve1' :: [Int] -> ZeroCounter
solve1' = foldl (flip turn') newZeroCounter

solve2 :: [Int] -> ZeroCounter
solve2 = foldl (flip turn'') newZeroCounter
