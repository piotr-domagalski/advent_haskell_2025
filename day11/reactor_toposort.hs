import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import Data.Char (isAlpha)
import Test.QuickCheck
import Data.Maybe (fromMaybe)

type MachineId = String
type Wiring = M.Map MachineId [MachineId]
type PathCounts = M.Map MachineId Int

parse :: String -> Wiring
parse = M.insert "out" [] . M.fromList . map parseMachine . lines

parseMachine :: String -> (MachineId, [MachineId])
parseMachine = fmap (words . dropWhile (not . isAlpha))
          . span (/=':')

swapDir :: Ord a => M.Map k [a] -> M.Map a [k]
swapDir = M.foldlWithKey mapFolder M.empty 
    where mapFolder :: Ord a => M.Map a [k] -> k -> [a] -> M.Map a [k]
          mapFolder m key succs = foldl (listFolder key) m succs
          listFolder :: Ord a => k -> M.Map a [k] -> a -> M.Map a [k]
          listFolder key = \m s -> M.insertWith (++) s [key] m

prop_swapDirTwiceEqId :: M.Map Int [Int] -> Property
prop_swapDirTwiceEqId m =
    (all ((/= 0) . length . snd) . M.toList) m
    ==>
    M.map S.fromList m === (M.map S.fromList $ swapDir $ swapDir m)

calcPredCounts :: Ord a => M.Map a [a] -> M.Map a Int
calcPredCounts m =
    let newmap = M.foldl (\newmap list ->
                          foldl (\newmap' elem ->
                                 M.insertWith (+) elem 1 newmap'
                                ) newmap list
                         ) M.empty m
        newmap' = foldl (\acc key -> M.insertWith (+) key 0 acc) newmap (M.keys m)
     in newmap'


topoSort :: (Show a, Ord a) => M.Map a [a] -> [a]
topoSort m = let pred_counts = calcPredCounts m
                 init_queue = Sq.fromList $ map fst $ filter ((==0) . snd) $ M.toList pred_counts
              in topoSort' init_queue pred_counts m

topoSort' :: (Show a, Ord a) => Sq.Seq a -> M.Map a Int -> M.Map a [a] -> [a]
topoSort' (first Sq.:<| queue) pred_counts succ_map =
    let succs = case M.lookup first succ_map of
                     Nothing -> error $ "not found in adj_map: " ++ (show first)
                     Just xs -> xs
        new_pred_counts = foldl (\acc next -> M.adjust pred next acc) pred_counts succs
        new_to_remove = filter (\x -> M.lookup x new_pred_counts == Just 0) succs
        new_queue = foldl (Sq.|>) queue new_to_remove
     in first:(topoSort' new_queue new_pred_counts succ_map)
     --in (first, succs, new_pred_counts, new_to_remove, new_queue):(topoSort' new_queue new_pred_counts succ_map)
topoSort' Sq.Empty _ _ = []

solve1 :: Wiring -> Int
solve1 wiring = let counted = countPaths "you" wiring
                 in fromMaybe 0 $ M.lookup "out" counted

solve2 :: Wiring -> Int
solve2 wiring = let paths1 = countPathsN ["svr", "fft", "dac", "out"] wiring
                    paths2 = countPathsN ["svr", "dac", "fft", "out"] wiring
                 in product paths1 + product paths2

countPathsN [] _ = []
countPathsN [_] _ = []
countPathsN (from:to:rest) adj_map =
    let subgraph = calcSubgraph from to adj_map
        paths = countPaths from subgraph
        count = fromMaybe 0 $ M.lookup to paths
     in count:countPathsN (to:rest) adj_map


countPaths :: (Show a, Ord a) => a -> M.Map a [a] -> M.Map a Int
countPaths from adj_map =
    let queue = topoSort adj_map
        init_path_counts = M.insert from 1 M.empty
     in countPaths' queue init_path_counts adj_map

countPaths' :: Ord a => [a] -> M.Map a Int -> M.Map a [a] -> M.Map a Int
countPaths' (next:queue) counts succ_map = 
    let succ_count = fromMaybe 0 $ M.lookup next counts
        succs = fromMaybe [] $ M.lookup next succ_map
        next_counts = foldl (\acc s -> M.insertWith (+) s succ_count acc) counts succs
     in countPaths' queue next_counts succ_map
countPaths' _ counts _ = counts

calcSubgraph :: Ord a => a -> a -> M.Map a [a] -> M.Map a [a]
calcSubgraph from to adj_map =
    let reachable_from = getReachableSubset [from] adj_map
        reachable_to = getReachableSubset [to] $ swapDir adj_map
        node_subset = reachable_from `S.intersection` reachable_to
     in M.fromList $
        map (\(k, vs) -> (k, filter (`S.member` node_subset) vs)) $
        filter ((`S.member` node_subset) . fst) $
        M.toList adj_map

getReachableSubset :: Ord a => [a] -> M.Map a [a] -> S.Set a
getReachableSubset queue adj_map = S.fromList $ getReachableList queue [] adj_map 

getReachableList :: Ord a => [a] -> [a] -> M.Map a [a] -> [a]
getReachableList [] list _ = []
getReachableList (next:queue) list adj_map =
    let adjs = fromMaybe [] $ M.lookup next adj_map
        -- these three get stuck in an infinite loop:
        -- next_queue = adjs ++ queue
        -- next_queue = queue ++ adjs
        -- next_queue = queue ++ (filter (not . (`elem` list)) adjs)
        -- but this works
        next_queue = (filter (not . (`elem` list)) adjs) ++ queue
     in next:getReachableList next_queue (next:list) adj_map

main = do
    file:_ <- getArgs
    main' file

main' file = do
    contents <- readFile file
    let parsed = parse contents
        solved1 = solve1 parsed
        solved2 = solve2 parsed
    putStrLn $ "Task 1: " ++ (show solved1)
    putStrLn $ "Task 2: " ++ (show solved2)
    
