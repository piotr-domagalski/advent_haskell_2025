main' :: String -> IO ()
main' file = do
   ans1 <- solve1 . parse <$> readFile file
   putStrLn $ show ans1

parse :: String -> [Int]
parse = map (read . \(x:xs) -> (if x == 'L' then '-' else ' '):xs) . lines

-- solve :: [Int] -> Int
solve1 = length
      . filter (==0)
      . scanl (\acc v -> (acc+v) `mod` 100) 50
