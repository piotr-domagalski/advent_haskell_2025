import qualified Data.List as L

type Pos = Int
type TimelineCount = Int
type SplitCount = Int

type Beam = (TimelineCount, Pos)
type Splitter = Pos
type Layer = [Splitter]

splitters :: [Splitter] -> Beam -> [Beam]
splitters list (count, pos)
    | pos `elem` list = [(count, pos-1), (count, pos+1)]
    | otherwise = [(count, pos)]

--dedupes a list of pairs by summing the 1st items of list elements whose 2nd element is equal
--assumes a list sorted in b, and this precondition is not checked
dedup_count :: (Num a, Eq b) => [(a, b)] -> [(a, b)]
dedup_count = map (\l@((_,pos):_) -> (sum $ map fst l, pos))
       . L.groupBy (\a b -> snd a == snd b)

-- splits beams with layer, keeping track of the total splits in splits_count
step :: (SplitCount, [Beam]) -> Layer -> (SplitCount, [Beam])
step (splits, beams) layer = let beams' = beams >>= splitters layer
                                 splits' = (length beams' - length beams)
                              in (splits+splits', dedup_count beams')

solve :: (Beam, [Layer]) -> [(Int, [Beam])]
solve (init_beam, layers) = scanl step (0, [init_beam]) layers

parse :: String -> (Beam, [Layer])
parse contents = let (start:splitters) = map snd $ filter (even . fst) $ enumerate $ lines contents
                     Just beam_pos = L.elemIndex 'S' start
                     layers = map (map fst . filter ((=='^') . snd) . enumerate) splitters
                  in ((1, beam_pos), layers)

enumerate :: [a] -> [(Int, a)]
enumerate (x:rest) = scanl (\(c,_) e -> (c+1, e)) (0,x) $ rest

main' file = do
    contents <- readFile file
    let parsed = parse contents
        solved = solve parsed
        (splits, beams) = last solved
    putStrLn $ "Task 1 (splits): " ++ (show splits)
    putStrLn $ "Task 2 (timelines): " ++ (show $ sum $ map fst $ beams)
