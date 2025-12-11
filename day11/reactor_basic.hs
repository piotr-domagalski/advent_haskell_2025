import System.Environment
import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    file:_ <- getArgs
    main' file

main' :: String -> IO ()
main' file = do
    contents <- readFile file
    let parsed = parse contents
        trace1 = solve parsed
        ans1 = getAns trace1
        (ans2, (c1, t1, c2, t2)) = solve2 parsed
    --putStrLn $ unlines $ map show $ M.toList $ parsed
    --putStrLn $ unlines $ map show $ solved
    --putStrLn $ show ans
    --putStrLn $ unlines $ map show $ M.toList $ parsed
    --putStrLn $ unlines $ map (unlines . map show) $ trace2
    --putStrLn $ unwords $ map (show . length . fst) $ trace1
    --putStrLn $ unlines $ map show $ c1
    --putStrLn $ unlines $ map show $ c2
    putStrLn $ show ans1
    --putStrLn $ show ans2

parse :: String -> Wiring
parse = M.fromList . map parseLine . lines

parseLine :: String -> (MachineId, [MachineId])
parseLine = fmap (words . dropWhile (not . isAlpha))
          . span (/=':')

type MachineId = String
type Wiring = M.Map MachineId [MachineId]
type PathCounts = M.Map MachineId Int

--solve :: Wiring -> Int
solve wiring = let trc = countPaths2 "you" "out" wiring
                in trc
getAns trc = fromMaybe 0 (M.lookup "out" $ snd $ last trc)

solve2 wiring = let ord1 = ["fft", "dac", "out"]
                    ord2 = ["dac", "fft", "out"]
                    (total1, counts1, trace1) = solveOrd ord1 wiring
                    (total2, counts2, trace2) = solveOrd ord2 wiring
                 in (total1+total2, (counts1, trace1, counts2, trace2))
    where solveOrd ord wiring = let counts = zip (tail ord) (countPathsN ord wiring)
                                    counts' = map (\(tgt, cs) -> fromMaybe 0 $ M.lookup tgt $ snd $ last cs) counts
                                    total = product counts'
                                 in (total, counts', counts)

fmtTrace2 (counts1, trace1, counts2, trace2) =
    let zipped1 = map (\(c, t) -> fst t ++ ": " ++ show c ++ "\n" ++ (unlines $ map show $ snd t)) $ zip counts1 trace1
        zipped2 = map (\(c, t) -> fst t ++ ": " ++ show c ++ "\n" ++ (unlines $ map show $ snd t)) $ zip counts2 trace2
     in unlines zipped1 ++ unlines zipped2

countPathsN :: [MachineId] -> Wiring -> [[([MachineId], PathCounts)]]
countPathsN (from:to:rest) wiring = (countPaths2 from to wiring):countPathsN (to:rest) wiring
countPathsN _ _ = []

countPaths2 :: MachineId -> MachineId -> Wiring -> [([MachineId], PathCounts)]
countPaths2 from to = countPaths' to [from] (M.fromList [(from,1)])

countPaths' :: MachineId -> [MachineId] -> PathCounts -> Wiring -> [([MachineId], PathCounts)]
countPaths' tgt [] path_counts wiring = [([], path_counts)]
countPaths' tgt (curr:queue) path_counts wiring
    | curr == "out" = countPaths' tgt queue path_counts wiring
    | otherwise = 
    let curr_paths :: Int    = case M.lookup curr path_counts of
                                    Nothing -> error $ "machine " ++ curr ++ " got in the queue, but no paths lead to it"
                                    Just x -> x
        nexts :: [MachineId] = case M.lookup curr wiring of
                                    Nothing -> error $ "key not found in wiring diagram: " ++ curr
                                    Just nexts -> nexts
        path_counts' :: PathCounts = foldl (\counts next -> M.insertWith (+) next 1 counts) path_counts nexts
     in (curr:queue, path_counts):(countPaths' tgt (nexts++queue) path_counts' wiring)
     --where types = (tgt :: MachineId, curr :: MachineId, queue :: [MachineId], path_counts :: PathCounts, wiring :: Wiring)
