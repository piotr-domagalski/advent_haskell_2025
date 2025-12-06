import Data.Char (isSpace)
import qualified Data.List as L

main' file = do
    contents <- readFile file
    let questions = parse contents
        answers = map solveQuestion questions
        total = sum answers
        questions2 = parse2 contents
        answers2 = map solveQuestion questions2
        total2 = sum answers2
    putStrLn $ "Task 1: " ++ (show total)
    putStrLn $ "Task 2: " ++ (show total2)

type Operator = Int -> Int -> Int
type Question = ([Int], Operator)

parse :: String -> [Question]
parse string = let columns = L.transpose $ map words $ lines string
                   questions = map (\col -> (map read $ init col, parseOp $ last col)) columns
                in questions

parse2 :: String -> [Question]
parse2 string = let columns = spans (not . all isSpace) $ L.transpose $ lines string
                    questions = map parseCol $ columns
                 in questions

parseCol :: [String] -> Question
parseCol col = let nums = map (read . init) col
                   op   = parseOp $ [last $ head col]
                in (nums, op)

parseOp :: String -> Operator
parseOp "*" = (*)
parseOp "+" = (+)
parseOp other = error $ "invalid operator: " ++ other


solveQuestion :: Question -> Int
solveQuestion (nums, op) = foldl1 op nums

spans :: (a -> Bool) -> [a] -> [[a]]
spans _ [] = []
spans pred list = case span pred list of
                       (first, [])     -> [first]
                       (first, _:tail) -> first:spans pred tail
