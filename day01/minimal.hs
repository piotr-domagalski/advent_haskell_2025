import System.Environment

main = getArgs >>= main' . head

main' file = show . solve1 . parse <$> readFile file >>= putStrLn

parse = map (read . \(x:xs) -> (if x == 'L' then '-' else ' '):xs) . lines

solve1 = length . filter (==0) . scanl (\acc v -> (acc+v) `mod` 100) 50
