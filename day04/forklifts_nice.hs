import qualified Data.Map as M
import qualified Grid as G
import qualified Data.Set as S

type Coords = (Int,Int)
type Grid a = M.Map Coords a

main' file = do
    contents <- readFile file
    let parsed = parse contents
        (w, h) = (length $ head $ lines contents, length $ lines contents)
        solved = solve parsed
        answers = map (length . snd) solved
    -- putStrLn $ unlines $ map (formatRow w h) solved
    putStrLn $ "Task 1 (immediately accesible): " ++ (show $ head answers)
    putStrLn $ "Task 2 (accesible after n moves): " ++ (show answers) ++ " = " ++ (show $ sum answers)

formatRow w h (g,q) = unlines [ show $ G.fromMapOfSizeWithDefault w h '.' $ M.map (\x -> if x >= 0 then '@' else '.') g
                              , show q
                              , "len = " ++ (show $ length q)]

parse :: String -> Grid Int
parse = calcInitialNeighbourCounts . M.fromList . parse' 0 0
    where parse' _ _ [] = []
          parse' x y ('\n':tail) = parse' 0 (y+1) tail
          parse' x y ('@':tail) = ((x,y), 0):parse' (x+1) y tail
          parse' x y ('.':tail) = parse' (x+1) y tail
          parse' x y (other:tail) = error $ "Invalid char: " ++ [other]

solve :: Grid Int -> [(Grid Int, S.Set Coords)]
solve grid = let queue = initQueue grid
              in solve_aux (grid,queue)
             where solve_aux :: (Grid Int, S.Set Coords) ->  [(Grid Int, S.Set Coords)]
                   solve_aux (g,q)
                    | null q    = []
                    | otherwise = (g,q):(solve_aux $ removeQueue g q)

initQueue :: Grid Int -> S.Set Coords
initQueue = S.fromList . map fst . filter (\(k,v) -> v<4) . M.toList 

removeQueue :: Grid Int -> (S.Set Coords) -> (Grid Int, S.Set Coords)
removeQueue grid queue = foldl (flip removeCoords) (grid, S.empty) queue

removeCoords :: Coords -> (Grid Int, S.Set Coords) -> (Grid Int, S.Set Coords)
removeCoords coords (grid, newqueue) = let grid' = adjustNeighbours pred coords $
                                                   M.delete coords $ grid
                                           to_remove = S.fromList $ filterNeighbours (<4) coords grid'
                                           newqueue' = newqueue `S.union` to_remove
                                        in (grid', S.filter (`M.member` grid') newqueue')

calcInitialNeighbourCounts :: Grid Int -> Grid Int
calcInitialNeighbourCounts grid = foldl (\grid' coords -> adjustNeighbours (+1) coords grid') grid $ M.keys grid

calcNeighbourCoords :: Coords -> [Coords]
calcNeighbourCoords (x,y) = [ (x-1,y-1),(x,y-1),(x+1,y-1)
                            , (x-1,y  ),        (x+1,y  )
                            , (x-1,y+1),(x,y+1),(x+1,y+1) ]

filterNeighbours :: (a -> Bool) -> Coords -> Grid a -> [Coords]
filterNeighbours fn coords grid = filter (filter_fn) $
                                  calcNeighbourCoords coords
        where filter_fn = \c -> case M.lookup c grid of
                                    Nothing -> False
                                    Just x -> fn x

adjustNeighbours :: (a -> a) -> Coords -> Grid a -> Grid a
adjustNeighbours fn coords grid = foldl (flip $ adjustNeighbour fn) grid $
                                  calcNeighbourCoords coords
adjustNeighbour :: (a -> a) -> Coords -> Grid a -> Grid a
adjustNeighbour fn coords grid = M.adjust fn coords grid
