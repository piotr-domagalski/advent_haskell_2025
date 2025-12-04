import qualified Grid as G

data Cell = Box | Empty

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
    print parsed

parse :: String -> G.Grid Cell
parse str = let rows = lines str
                w = length $ head rows
                h = length rows
                l = map charToCell $ filter (/='\n') str
             in G.fromList w h l
