import System.Environment
import Data.Char (ord)
import Data.List (findIndex, intersperse, elemIndex, zip4)

main = do
    file:_ <- getArgs
    main' file

main' file = do
    contents <- readFile file
    putStrLn $ formatAnswer (solve 2 contents, solve 12 contents)

formatAnswer :: (Int, Int) -> String
formatAnswer (max2,max12) = "Task 1 (max of 2):  " ++ (show max2) ++
                          "\nTask 2 (max of 12): " ++ (show max12)

-- parsing
parse :: String -> [[Int]]
parse = map parseBank . lines

parseBank :: String -> [Int]
parseBank = map (\c -> ord c - ord '0')

-- logic
solve :: Int -> String -> Int
solve n = sum . map (digitsToNum . foldr (updateJoltageList n) []) . parse

calcMaxJoltage :: Int -> [Int] -> [[Int]]
calcMaxJoltage n = scanr (updateJoltageList n) []

updateJoltageList :: Int -> Int -> [Int] -> [Int]
updateJoltageList n x acc
    | length acc < n = x:acc
    | (beg, _:end) <- splitBeforeLastSorted $ x:acc = beg++end
    | otherwise = undefined

sortedUntilIndex :: Ord a => [a] -> Int
sortedUntilIndex [] = 0
sortedUntilIndex [_] = 1
sortedUntilIndex (f:s:tail)
    | s > f = 1
    | otherwise = succ $ sortedUntilIndex (s:tail)

splitBeforeLastSorted :: Ord a => [a] -> ([a], [a])
splitBeforeLastSorted list = splitAt (pred $ sortedUntilIndex list) list

digitsToNum :: Num a => [a] -> a
digitsToNum = foldl (\acc a -> acc*10+a) 0
