import qualified Data.Set as S
import qualified Data.List as L

type Beam = (Int, Int)
type Splitter = Int
type Layer = [Splitter]

main' file = do
    parsed <- parse <$> readFile file
    let steps = solve parsed
        ans1 = fst $ last steps
        ans2 = sum $ map fst $ snd $ last steps
    putStrLn $ unlines $ map show steps
    putStrLn $ "Task 1: " ++ (show ans1)
    putStrLn $ "Task 2: " ++ (show ans2)

parse :: String -> (Beam, [Layer])
parse contents = let (start:splitters) = map snd $ filter (even . fst) $ enumerate $ lines contents
                     Just beam_pos = L.elemIndex 'S' start
                     layers = map (map fst . filter ((=='^') . snd) . enumerate) splitters
                  in ((1, beam_pos), layers)

solve :: (Beam, [Layer]) -> [(Int, [Beam])]
solve (init_beam, layers) = scanl map_layer (0, [init_beam]) layers


split_beam :: Layer -> Beam -> (Int, [Beam])
split_beam layer (timelines, beam) = if any (==beam) layer then (1, [(timelines,beam-1),(timelines,beam+1)]) else (0, [(timelines, beam)])

map_layer :: (Int, [Beam]) -> Layer -> (Int, [Beam])
map_layer (count, beams) layer = let (counts, newbeams) = unzip $ map (split_beam layer) beams
                                     beams_dedup = map count_timelines $ L.groupBy (\a b -> snd a == snd b) $ concat $ newbeams
                                     total = sum counts
                                  in (count + total, beams_dedup)
        where count_timelines :: [Beam] -> Beam
              count_timelines = (foldl1 (\(acc, pos) (count, _) -> (acc+count, pos)))
              same_pos :: Beam -> Beam -> Bool
              same_pos a b = snd a == snd b

enumerate :: [a] -> [(Int, a)]
enumerate (x:rest) = scanl (\(c,_) e -> (c+1, e)) (0,x) $ rest
