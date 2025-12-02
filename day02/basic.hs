import System.Environment
import Data.List

--main :: IO ()
--main = getArgs >>= main' debug . head

-- main' :: ([Counter] -> String) -> String -> IO ()
main' format file = do
        contents <- readFile file
        let input = parse contents
        putStrLn $ contents
        putStrLn $ show input
        let ans = solve input
        putStrLn $ "two repeats: " ++ show ans
        let ans2 = solve2 input
        putStrLn $ "any repeats: " ++ show ans2


debug :: Show a => [a] -> String
debug = unlines . map show

-- parse [] = []
parse :: String -> [(Int, Int)]
parse = map (\a -> let (from,_:to) = span (/='-') a in (read from, read to) )
      . filter (/=",")
      . groupBy (\a b -> (a/=',')==(b/=','))

solve = sum
      . filter (\a -> let (_,_,_,b) = isSilly a in b)
      . concat
      . map (\(from, to) -> [from..to])
    where isSilly :: Int -> (Int, Int, Int, Bool)
          isSilly n
            | odd $ digits = (n, 0, 0, False)
            | otherwise = (n, firstN', lastN, firstN' == lastN)
            where digits = succ . truncate . logBase 10 . fromIntegral $ n
                  shift = 10^(digits`div`2)
                  firstN = n `rem` (shift)
                  lastN = n - firstN
                  firstN' = firstN * shift

solve2 :: [(Int, Int)] -> Int
solve2 = sum
       . map read
       . filter isSilly'
       . map show
       . concat
       . map (\(from, to) -> [from..to])

isSilly' :: String -> Bool
isSilly' str = foldl (||) False
             . map (\i -> isPrefixOf str $ cycle $ take i str)
             . divisors . length $ str

divisors n = filter ((==0) . (n `rem`)) [1..n`div`2]
