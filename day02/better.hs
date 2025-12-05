import Data.List
import Test.QuickCheck

digitsCount 0 = 1
digitsCount n = aux n 0
  where aux n c = if (n `rem` 10^c) == n then c else aux n (c+1)

prop_digitsCountEqLength x = digitsCount x == (length $ show $ abs $ x)
  where types = (x :: Int)

divisors n = filter ((==0) . (n `rem`)) [1..n`div`2]

type Range = (Int, Int)
parse :: String -> [Range]
parse = map (\a -> let (from,_:to) = span (/='-') a in (read from, read to) )
      . filter (/=",")
      . groupBy (\a b -> (a/=',')==(b/=','))

main' file = (\x -> "Task 2: " ++ show x) . solve . parse <$> readFile file >>= putStrLn

solve :: [Range] -> Int
solve ranges = let split = concat $ map splitRange ranges
                   sillies = map getSillies split
                   uniques = map head . group . sort . concat $ sillies
                   total = sum uniques
                in total

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
                      map (\(len, (from, to)) -> (getSeed len ((digitsCount $ fst range) `div` len) *) <$> [from..to]) $
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
