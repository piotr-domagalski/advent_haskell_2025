import System.Environment

main :: IO ()
main = getArgs >>= main' . head

main' :: String -> IO ()
main' file = show . solve1 . parse <$> readFile file >>= putStrLn

parse :: String -> [Int]
parse = map (read . \(x:xs) -> (if x == 'L' then '-' else ' '):xs) . lines

solve1 :: [Int] -> Int
solve1 = length . filter (==0) . scanl (\acc v -> (acc+v) `mod` 100) 50
