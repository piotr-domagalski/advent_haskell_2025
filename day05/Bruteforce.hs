module Bruteforce where

import System.Environment
import qualified Data.Set as S

type Id = Int
type Range = (Id, Id)

main = do
    file:_ <- getArgs
    main' file

main' :: FilePath -> IO ()
main' file = do
    contents <- readFile file
    let (ranges, ids) = parse contents
        valids = constructSetValids ranges
        count = length $ filter (`S.member` valids) ids
    print ranges
    print ids
    print valids
    putStrLn $ "len ranges = " ++ (show $ length ranges)
    putStrLn $ "len ids = " ++ (show $ length ids)
    putStrLn $ "len valids = " ++ (show $ length valids)
    print count

solve :: ([Range], [Id]) -> Int
solve (ranges, ids) = let valids = constructSetValids ranges
                       in length $ filter (`S.member` valids) ids

parse :: String -> ([Range], [Id])
parse contents = let (ranges, _:ids) = span (/="") $ lines contents
                     ranges' = map parseRange ranges
                     ids' = map read ids
                  in (ranges', ids')

parseRange :: String -> Range
parseRange str = let (from, _:to) = span (/='-') $ str
                  in (read from, read to)

constructSetValids :: [Range] -> S.Set Id
constructSetValids = S.unions . map (\(from,to) -> S.fromAscList [from..to])
