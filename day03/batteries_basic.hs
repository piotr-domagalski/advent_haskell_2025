import System.Environment (getArgs)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, zip4)

main :: IO ()
main = getArgs >>= main' . head

main' :: String -> IO ()
main' file = do
    contents <- readFile file
    let parsed = parse contents
        scanned = map (scanl updateJoltage initJoltage . init) parsed
        maxes = map (joltageToInt . calcMaxJoltage) parsed
        maxes' = map calcMaxJoltageBad parsed
    putStrLn $ unlines $ map show $ filter (\(_,_,a,b) -> a /= b) $ zip4 (lines contents) scanned maxes maxes'
    putStrLn $ show $ sum maxes
    putStrLn $ show $ sum maxes'

--debug = unlines . map show

parse :: String -> [[Int]]
parse = map parseBank . lines

parseBank :: String -> [Int]
parseBank = map (\c -> ord c - ord '0')

type Joltage = (Maybe Int, Maybe Int)
initJoltage = (Nothing, Nothing)
joltageToInt :: Joltage -> Int
joltageToInt (Just a, Just b) = 10*a+b
joltageToInt nothing = error $ "Cannot calculate joltage for " ++ show nothing

solve :: [[Int]] -> Int
solve = sum . map (joltageToInt . calcMaxJoltage)

--solve' = sum . map (joltageToInt . foldl updateJoltage initJoltage)
calcMaxJoltage :: [Int] -> Joltage
calcMaxJoltage xs = let (hi, lo) = foldl updateJoltage initJoltage $ init xs
                    in (hi, Just (max (last xs) (fromMaybe 0 lo)))

updateJoltage :: Joltage -> Int -> Joltage
updateJoltage (Nothing, x) y = (Just y, x)
updateJoltage (Just x, Nothing) y
    | y > x     = (Just y, Nothing)
    | otherwise = (Just x, Just y)
updateJoltage (Just x, Just y) z
    | z > x     = (Just z, Nothing)
    | z > y     = (Just x, Just z)
    | otherwise = (Just x, Just y)

calcMaxJoltageBad :: [Int] -> Int
calcMaxJoltageBad xs = let hi = maximum $ init xs
                           idx = case elemIndex hi xs of
                                Nothing -> error "aaa"
                                Just i -> i
                           (_, rest) = splitAt (idx+1) xs
                           lo = maximum rest
                       in
                           hi*10+lo
