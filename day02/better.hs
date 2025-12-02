import Data.List
import Test.QuickCheck

digits :: String -> String -> [String]
digits from to = reverse $ digits' from to []

digits' :: String -> String -> [String] -> [String]
digits' [] [] acc = acc
digits' f t []
    | (f', t') <- (read f, read t) :: (Int,Int), f' > t' = error "from greater than to"
digits' (_:_) [] _ = error "strings not of equal length"
digits' [] (_:_) _ = error "strings not of equal length"
digits' (f:from) (t:to) [] = digits' from to [[f..t]]
digits' (f:from) (t:to) (prev:acc)
    | lp == 1 = digits' from to ([f..t]:prev:acc)
    | lp == 2 = digits' from to (ds:prev:acc)
    | otherwise = digits' from to (['0'..'9']:prev:acc)
    where lp = length prev
          ds = sort $ union [f..'9'] ['0'..t]

repeat `isSeedOf` xs = xs `isPrefixOf` cycle repeat && length xs `rem` length repeat == 0

digitsCount 0 = 1
digitsCount n = aux n 0
  where aux n c = if (n `rem` 10^c) == n then c else aux n (c+1)
digitsCount' = succ . truncate . logBase 10 . abs . fromIntegral

prop_digitsCountEqLength x = digitsCount x == (length $ show $ abs $ x)
  where types = (x :: Int)

divisors n = filter ((==0) . (n `rem`)) [1..n`div`2]

prefixes str = map (\i -> take i str) $ divisors $ length str

type Range = (Int, Int)
parse :: String -> [Range]
parse = map (\a -> let (from,_:to) = span (/='-') a in (read from, read to) )
      . filter (/=",")
      . groupBy (\a b -> (a/=',')==(b/=','))

main' file = do
    ranges <- parse <$> readFile file
    print ranges
    let splitRanges = map splitRange ranges
    let splitRanges' = concat splitRanges
    let prefixRanges = map getPrefixRanges splitRanges'
    putStrLn $ unlines $ map show $ zip ranges splitRanges
    putStrLn $ unlines $ map show $ zip splitRanges' prefixRanges
    let candidates = map (\range -> (range, getCandidates range)) splitRanges'
    putStrLn $ unlines $ map show $ candidates
    let sillies = map getSillies splitRanges'
    putStrLn $ unlines $ map show $ sillies
    let sillies' = map head . group . sort . concat $ sillies
    putStrLn $ unlines $ map show $ sillies'
    let ans = sum sillies'
    putStrLn $ "silly sum: " ++ (show ans)


splitRange :: Range -> [Range]
splitRange (from, to)
    | df == dt = [(from, to)]
    | otherwise = (from, to'):splitRange (from', to)
    where df = digitsCount from
          dt = digitsCount to
          from' = 10^df
          to' = from' - 1

getSillies :: Range -> [Int]
getSillies range = filter (filterCandidates range) $ getCandidates range

filterCandidates :: Range -> Int -> Bool
filterCandidates (from, to) maybesilly = maybesilly >= from && maybesilly <= to

-- getCandidates :: Range -> [Int]
getCandidates range = concat $
                      map (\(len, prefixes) -> (getSeed len ((digitsCount $ fst range) `div` len) *) <$> prefixes ) $
                      map (\(len, (from, to)) -> (len, [from..to])) $
                      getPrefixRanges range


getPrefixRanges :: Range -> [(Int, (Int,Int))]
getPrefixRanges (from, to)
  | digitsCount from /= digitsCount to = error "from and to must have the same number of digits"
  | otherwise = let
      prefix_lengths = divisors $ digitsCount from
      prefixes = zip prefix_lengths $ zip (map (getPrefix from) prefix_lengths) (map (getPrefix to) prefix_lengths)
      in prefixes

getPrefix :: Int -> Int -> Int
getPrefix num len = (abs $ num) `div` (10 ^ ((digitsCount num) - len))

prop_getPrefixEqTake num = forAll (choose (1, digitsCount num)) $ \len -> (show $ getPrefix num len) === (take len $ show $ abs num)

getSeed :: Int -> Int -> Int
getSeed _ 1 = 1
getSeed prefix_len count = (getSeed prefix_len (count-1)) * 10^(prefix_len) + 1
