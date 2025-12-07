import qualified Data.Set as S
import qualified Data.List as L

type Beam = Int
type Splitter = Int
type Layer = [Splitter]

main' file = do
    ans <- fst . last . solve . parse <$> readFile file
    putStrLn $ "Task 1: " ++ (show ans)

parse :: String -> (Beam, [Layer])
parse contents = let (start:splitters) = map snd $ filter (even . fst) $ enumerate $ lines contents
                     Just beam = L.elemIndex 'S' start
                     layers = map (map fst . filter ((=='^') . snd) . enumerate) splitters
                  in (beam, layers)

solve :: (Beam, [Layer]) -> [(Int, [Beam])]
solve (init_beam, layers) = scanl map_layer (0, [init_beam]) layers


split_beam :: Layer -> Beam -> (Int, [Beam])
split_beam layer beam = if any (==beam) layer then (1, [beam-1,beam+1]) else (0, [beam])

map_layer :: (Int, [Beam]) -> Layer -> (Int, [Beam])
map_layer (count, beams) layer = let (counts, newbeams) = unzip $ map (split_beam layer) beams
                                     beams_dedup = map head $ L.group $ concat $ newbeams
                                     total = sum counts
                                  in (count + total, beams_dedup)

enumerate :: [a] -> [(Int, a)]
enumerate (x:rest) = scanl (\(c,_) e -> (c+1, e)) (0,x) $ rest
