import qualified Bruteforce as B
import qualified Data.List as L

type Id = Int

newtype Range = Range {getRange :: (Id, Id)} deriving(Eq, Ord)
newRange :: Int -> Int -> Range
newRange from to
    | from > to = error "cannot construct range with from > to"
    | otherwise = Range {getRange = (from, to)}

instance Read Range where
    readsPrec prec str = let (from_str, _:to_str) = span (/= '-') str
                             [(to_id, rest)] = readsPrec prec to_str :: [(Id, String)]
                          in [(newRange (read from_str) to_id, rest)]

instance Show Range where
    show (Range {getRange=(from, to)}) = (show from) ++ "-" ++ (show to)

isIn :: Id -> Range -> Bool
id `isIn` range = let (from, to) = getRange range
                 in id >= from && id <= to

mergeMaybe :: Range -> Range -> Maybe Range
mergeMaybe lhs@(Range {getRange = (lhs_from, lhs_to)}) rhs@(Range {getRange = (rhs_from, rhs_to)})
    | lhs_to `isIn` rhs && lhs_from `isIn` rhs = Just rhs
    | lhs_to `isIn` rhs                        = Just (newRange lhs_from rhs_to)
    | rhs_to `isIn` lhs && rhs_from `isIn` lhs = Just lhs
    | rhs_to `isIn` lhs                        = Just (newRange rhs_from lhs_to)
    | otherwise = Nothing

mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges (a:b:tail)
    | Just ab <- mergeMaybe a b = mergeRanges $ ab:tail
    | Nothing <- mergeMaybe a b = a:(mergeRanges $ b:tail)

idCompareRange :: Id -> Range -> Ordering
id `idCompareRange` range@(Range {getRange=(from, to)})
    | id `isIn` range = EQ
    | id < from = LT
    | id > to = GT

size :: Range -> Int
size (Range {getRange=(from, to)}) = to - from + 1

parse :: String -> ([Range], [Id])
parse contents = let (ranges, _:ids) = span (/="") $ lines contents
                     ranges' = map read ranges
                     ids' = map read ids
                  in (ranges', ids')

main' file = do
    contents <- readFile file
    let (ranges, ids) = parse contents
        sorted = L.sort ranges
        merged = mergeRanges sorted
        freshCount = length $ filter id $ map (\id -> any (id `isIn`) ranges) ids
        idCount = sum $ map size merged

    putStrLn $ "Task 1 (fresh ingredients): " ++ (show freshCount)
    putStrLn $ "Task 2 (fresh ids):         " ++ (show idCount)

