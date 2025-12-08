import qualified Data.Map as M
import qualified Data.List as L

type Distance = Float
type Position = (Int, Int, Int)

example = [ (162,817,812)
          , (57,618,57)
          , (906,360,560)
          , (592,479,940)] :: [Position]

distance2 :: Position -> Position -> Float
distance2 (x1,y1,z1) (x2,y2,z2) = dx**2 + dy**2 + dz**2
    where dx = fromIntegral (x1-x2)
          dy = fromIntegral (y1-y2)
          dz = fromIntegral (z1-z2)

type Connection = (Distance, Position, Position)

type CircuitId = Int
type Circuits = (CircuitId, M.Map Position CircuitId)
emptyCircuits = (0, M.empty)

addConnection :: Circuits -> Connection -> Circuits
addConnection (nextid, nets) (_, from, to) =
        case (from_id, to_id) of
            (Just f, Just t)   -> (nextid, M.map (\o -> if o == t then f else o) nets)
            (Just f, Nothing)  -> (nextid, M.insert to f nets)
            (Nothing, Just t)  -> (nextid, M.insert from t nets)
            (Nothing, Nothing) -> (nextid+1, M.insert to nextid $ M.insert from nextid nets )
    where from_id = M.lookup from nets
          to_id = M.lookup to nets

calcConnections :: [Position] -> [Connection]
calcConnections [] = []
calcConnections (x:rest) = aux x rest ++ (calcConnections rest)
    where aux x = foldl (\acc y -> ((distance2 x y), x, y):acc) []

calcCircuits :: [Connection] -> [Circuits]
calcCircuits = scanl addConnection emptyCircuits

countCircuitMembers :: Circuits -> M.Map CircuitId Int
countCircuitMembers = M.foldl (\counts id -> M.insertWith (+) id 1 counts) M.empty . snd

--solve :: [Position] -> ??
solve n positions = let circuits = calcCircuits . take n . L.sort . calcConnections $ positions
                        counts = countCircuitMembers . last $ circuits
                        product = foldl (*) 1 . take 3 . L.sortOn negate . map snd . M.toList $ counts
                     in (product, (circuits, counts))

parse :: String -> [Position]
parse = map (\s -> read ('(':s++")")) . lines

main' file = do
    contents <- readFile file
    let parsed = parse contents
        (ans1, (circuits, counts)) = solve 1000 parsed
    putStrLn $ "Task 1: " ++ (show ans1)
