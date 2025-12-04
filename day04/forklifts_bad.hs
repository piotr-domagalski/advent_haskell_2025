import qualified Grid as G

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
    print solved


parse :: String -> G.Grid Cell
parse str = let rows = lines str
                w = length $ head rows
                h = length rows
                l = map charToCell $ filter (/='\n') str
             in G.fromList w h l

solve :: G.Grid Cell -> Int
solve = sum . G.toList . fmap isAccessible . countNeighbours

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
