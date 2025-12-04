import qualified Grid as G
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as S
import Data.Maybe (isJust)

data Cell = Empty | Box deriving(Eq, Ord)

cellToChar :: Cell -> Char
cellToChar Box = '@'
cellToChar Empty = '.'
charToCell :: Char -> Cell
charToCell c
    | c == (cellToChar Box) = Box
    | c == (cellToChar Empty) = Empty
    | otherwise = error $ "invalid cell char: '" ++ c:"'"

instance Show Cell where
    show c = [cellToChar c]
instance Read Cell where
    readsPrec _ "" = []
    readsPrec _ (ch:tail) =[(charToCell ch, tail)]

main' file = do
    contents <- readFile file
    let parsed = parse contents
        solved = solve parsed
        solved2 = map (\(count, _) -> count) $ solve2 parsed
    print solved
    putStrLn $ (show solved2) ++ " = " ++ (show $ sum solved2)

parse :: String -> G.Grid Cell
parse str = let rows = lines str
                w = length $ head rows
                h = length rows
                l = map charToCell $ filter (/='\n') str
             in G.fromList w h l

type Coords = (Int, Int)
type KVList = [(Coords, Int)]
gridToMap :: G.Grid Cell -> M.Map Coords Int
gridToMap g@(G.Grid w h _) = M.fromList $ (\(_,_,kvList) -> kvList) $ foldl (aux w h) (0,0,[]) $ countNeighbours g
    where aux :: Int -> Int -> (Int,Int,KVList) -> Maybe Int -> (Int,Int,KVList)
          aux w h (x,y,acc) cell = let x' = if (x==w-1) then 0 else (succ x)
                                       y' = if (x==w-1) then succ y else y
                                    in case cell of
                                           Nothing -> (x',y',acc)
                                           Just c -> (x',y',((x,y),c):acc)

--solve2' :: G.Grid Cell -> [Int]
{-
solve2' g = let m = gridToMap g
                initq = initQueue m
             in aux m initq S.empty
    where aux :: M.Map Coords Int -> S.Set Coords -> S.Set Coords -> (M.Map Coords Int, S.Set Coords, S.Set Coords)
          aux m q v = scanl (update m v) (M.empty, S.empty, S.empty) (S.toAscList q)
          update m v (m', q', v') e = case M.lookup e m of
                                      Nothing -> (m', q', v')
                                      Just x -> if x > 4
                                                then (m', q', v')
                                                else (decNeighbours m' e, q', v')
-}


initQueue :: M.Map Coords Int -> S.Set Coords
initQueue = S.fromList . M.keys . M.filter (<=4)

getNeighbourKeys (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1)
                       ,(x-1,y  ),        (x+1,y  )
                       ,(x-1,y+1),(x,y+1),(x+1,y+1)]

iter file = do 
    contents <- readFile file
    let parsed@(G.Grid w h _) = parse contents
        solved2 = solve2 parsed
        m = gridToMap parsed
        q = initQueue m
        results = (m,q):recurse (m,q)
        -- results_strings = map (\(m,q) -> show (mapToGrid w h m) ++ "\n" ++ show q ++ "\nlen=" ++ show (length q) ++ "\n") results
        -- solved2strings = map (\(c,g) -> "should've removed=" ++ show c ++ "\n" ++ show g ++ "\n") solved2
    -- putStrLn $ unlines $ map (\(a,b) -> a ++ b) $ zip results_strings solved2strings
    -- putStrLn $ "Should've removed: " ++ show (map fst solved2) ++ " = " ++ (show (sum $ map fst solved2))
    putStrLn $ "Actually removed:  " ++ (show $ map (length . snd) results) ++ " = " ++ (show $ sum $ map (length . snd) results)


recurse (m, q)
    | q == S.empty = []
    | otherwise = let (m',q') = foldl queueFolder (m, S.empty) q
                   in (m',q'):(recurse (m',q'))

mapToGrid w h m = G.fromList w h $
                  map (\cs -> if M.member cs m then Box else Empty ) $
                  [(x,y) | y <- [0..w-1], x <- [0..h-1]]

queueFolder :: (M.Map Coords Int, S.Set Coords) -> Coords -> (M.Map Coords Int, S.Set Coords)
queueFolder (m, q) c = let m' = decNeighbours c . M.delete c $ m
                           q' = q `S.union` findAccessibleNeighbours c m'
                        in (m', S.filter (\c -> isJust $ M.lookup c m') q')


findAccessibleNeighbours :: (Int, Int) -> M.Map Coords Int -> S.Set Coords
findAccessibleNeighbours coords m = S.fromList
                                  . filter (\cs -> case M.lookup(cs) m of
                                                        Nothing -> False
                                                        Just x -> x <= 4
                                           ) $ getNeighbourKeys coords
decNeighbours :: (Int, Int) -> M.Map Coords Int -> M.Map Coords Int
decNeighbours coords m = foldl (\m' cs -> M.adjustWithKey (\k v -> pred v) cs m') m $ getNeighbourKeys coords

solve :: G.Grid Cell -> Int
solve = sum . G.toList . fmap isAccessible . countNeighbours

solve2 :: G.Grid Cell -> [(Int, G.Grid Cell)]
solve2 g
    | 0 == solve g = []
    | otherwise = (solve g, removeAccessible g):(solve2 $ removeAccessible g)

removeAccessible :: G.Grid Cell -> G.Grid Cell
removeAccessible = fmap aux . countNeighbours
    where aux :: Maybe Int -> Cell
          aux Nothing = Empty
          aux (Just x) = if x <=4 then Empty else Box


countNeighbours :: G.Grid Cell ->  G.Grid (Maybe Int)
countNeighbours g@(G.Grid w h _) = fmap (aux) $
                                   G.fromList w h $
                                   G.gridWindowsFill3x3 Empty g
                                   where aux :: G.Grid Cell -> Maybe Int
                                         aux g
                                            | G.gridAt 1 1 g == Just Box =
                                                Just (sum $ fmap (\(c) -> if c == Box then 1 else 0) g)
                                            | otherwise = Nothing

isAccessible :: Maybe Int -> Int
isAccessible c
    | Nothing <- c = 0
    | Just n <- c, n <=4 = 1
    | otherwise = 0
