import Data.Char (isSpace)
import qualified Data.List as L

main' file = do
    contents <- readFile file
    let questions = parse contents
        answers = map solveQuestion questions
        total = sum answers
    print $ answers
    print $ total

type Operator = Int -> Int -> Int
type Question = ([Int], Operator)

parse :: String -> [Question]
parse string = let columns = L.transpose $ map words $ lines string
                   questions = map (\col -> (map read $ init col, parseOp $ last col)) columns
                in questions
parseOp :: String -> Operator
parseOp "*" = (*)
parseOp "+" = (+)
parseOp other = error $ "invalid operator: " ++ other

solveQuestion :: Question -> Int
solveQuestion (nums, op) = foldl1 op nums
