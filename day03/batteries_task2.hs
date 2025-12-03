import Data.Char (ord)
import Data.List (findIndex, intersperse, elemIndex, zip4)

main' file = do
    contents <- readFile file
    let parsed = parse contents
        scanned = map (calcMaxJoltage 12) parsed
        solved = map (digitsToNum . head) scanned
        --solvedbad = map (digitsToNum . task2bad 12) parsed
        --solvedverybad = map (task2verybad 3) parsed
        strings = map (\(a,b,c,_) -> showCase a b c) $
                  filter (\(_,_,a,b) -> a/=b) $
                  zip4 parsed scanned solved solved
    putStrLn $ show $ sum solved

showCase :: (Show a, Show b, Show c) => a -> [b] -> c -> String
showCase parsed scanned output =
    unlines [ (show parsed), ""
            , (unlines $ map show scanned)
            , show output
            ]

parse :: String -> [[Int]]
parse = map parseBank . lines

parseBank :: String -> [Int]
parseBank = map (\c -> ord c - ord '0')

calcMaxJoltage n = scanr (updateJoltageList n) []

updateJoltageList :: Int -> Int -> [Int] -> [Int]
updateJoltageList n x acc
    | length acc < n = x:acc
    {-
    | head acc <= x
    , Just smaller <- findIndex (== minimum acc) acc
      = let (beg, _:end) = splitAt smaller acc
        in x:beg ++ end
    -}
    | (beg, _:end) <- splitBeforeLastSorted $ x:acc = beg++end
    | otherwise = acc

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc a -> acc*10+a) 0

task2bad :: Int -> [Int] -> [Int]
task2bad 0 _ = []
task2bad n [] = error "bbb"
task2bad n xs = let hi = maximum xs
                    idx = case elemIndex hi xs of
                         Nothing -> error "aaa"
                         Just i -> i
                    (_, rest) = splitAt (idx+1) xs
                in
                    hi:task2bad (n-1) rest

task2verybad :: Int -> [Int] -> Int
task2verybad n xs = maximum $ map digitsToNum $ allPossibilities n xs

allPossibilities :: Int -> [Int] -> [[Int]]
allPossibilities n xs = aux (n-1) $ allDeleteAts xs
    where aux :: Int -> [[Int]] -> [[Int]] 
          aux 0 xs = xs
          aux n xs = aux (n-1) $ concat $ map allDeleteAts xs

allDeleteAts xs = [deleteAt idx xs | idx <- [0..(length xs-1)]]

deleteAt idx xs = ls ++ rs
    where (ls, _:rs) = splitAt idx xs

intToDigits num = map (\c -> ord c - ord '0') $ show num
nDigitInts n = [10^(n-1)..10^n-1]
testCases length = filter (not . (0 `elem`)) $ map intToDigits $ nDigitInts length

runTests in_length choose = do
        let cases = testCases in_length
            bruteforce = map (task2verybad (in_length-choose)) cases
            scanned = map (calcMaxJoltage choose) cases
            solved = map (digitsToNum . head) scanned
            zipped = zip4 cases scanned solved bruteforce 
            filtered = filter (\(_,_,a,b) -> a/=b) zipped
        putStrLn $ unlines $ map show $ filtered
        putStrLn $ "wrong solutions: " ++ (show $ length filtered)

sortedUntilIndex :: Ord a => [a] -> Int
sortedUntilIndex [] = 0
sortedUntilIndex [_] = 1
sortedUntilIndex (f:s:tail)
    | s > f = 1
    | otherwise = succ $ sortedUntilIndex (s:tail)

splitBeforeLastSorted :: Ord a => [a] -> ([a], [a])
splitBeforeLastSorted list = splitAt (pred $ sortedUntilIndex list) list
    
