module LinearSolver where

import qualified Data.List as L
import Text.Pretty.Simple as PP
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Debug.Pretty.Simple (pTraceOpt)

type MatrixVal = Rational
mypprintopts = PP.defaultOutputOptionsDarkBg { outputOptionsCompact = True }

mypprint :: Show a => a -> IO ()
mypprint = PP.pPrintOpt NoCheckColorTty mypprintopts

myptrace :: String -> a -> a
myptrace = pTraceOpt NoCheckColorTty mypprintopts

data EqSysMatrix = Matrix { getM :: [[MatrixVal]], getColIds :: [Int], getEqPos :: Int } deriving (Show, Eq)
emptyMatrix = Matrix { getM = [], getColIds = [], getEqPos = 0 }

test_matrix1 =
    Matrix { getM = [ [0,  1,  2,  3,  4]
                    , [1,  2,  3,  4,  5]
                    , [2, 10, 15, 12, 12]]
           , getColIds = [0,1,2,3,4]
           , getEqPos = 4 }

test_matrix2 =
    Matrix { getM = [ [0, 0, 0, 0, 1, 1, 3]
                    , [0, 1, 0, 0, 0, 1, 5]
                    , [0, 0, 1, 1, 1, 0, 4]
                    , [1, 1, 0, 1, 0, 0, 7]]
           , getColIds = [0,1,2,3,4,5]
           , getEqPos = 6 }

test_matrix3 =
    Matrix { getM = [ [0, 0, 0, 0, 1, 1,  3]
                    , [0, 1, 0, 0, 0, 1,  5]
                    , [0, 2, 0, 0, 0, 2, 10]
                    , [0, 0, 1, 1, 1, 0,  4]
                    , [1, 1, 0, 1, 0, 0,  7]]
           , getColIds = [0,1,2,3,4,5]
           , getEqPos = 6 }

data Solutions = Solutions { getBaseVals :: [MatrixVal], getFreeVars :: [(Int, [MatrixVal])] } deriving (Show, Eq)
emptySolutions = Solutions { getBaseVals = [], getFreeVars = [] }

getColumn :: Int -> [[a]] -> [a]
getColumn i = map (\xs -> xs !! i)

gaussianElim :: EqSysMatrix -> Maybe EqSysMatrix
gaussianElim m = let steps = gaussianElimSteps $ Just m
                  in if null steps
                     then Nothing
                     else Just $ (\(_,m) -> m) $ last steps

--gaussianElimSteps :: Maybe EqSysMatrix -> [EqSysMatrix]
gaussianElimSteps Nothing = []
gaussianElimSteps (Just m) =
    let last_nonzero = pred $ length $ getM m
     in _gaussianElimSteps 0 last_nonzero (Just m)

--_gaussianElimSteps :: Int -> Int -> Maybe EqSysMatrix -> [EqSysMatrix]
_gaussianElimSteps _ _ Nothing = []
_gaussianElimSteps curr_i last_i (Just matrix)
    | curr_i == (length $ getM matrix) = [(show ("done", -1, -1), matrix)]
    | otherwise =
    case findIdxOfRowWithColNonzero curr_i curr_i matrix of
        Just idx ->
            if idx == curr_i
            then (show ("zeroing", curr_i, idx), matrix):_gaussianElimSteps (curr_i+1)
                                                                            last_i
                                                                            (Just $ zeroColUsingRow curr_i curr_i matrix)

            else (show ("swaprow", curr_i, idx), matrix):_gaussianElimSteps curr_i
                                                                      last_i
                                                                      (Just $ swapRows idx curr_i matrix)
        Nothing ->
            case findIdxOfColWithRowNonzero curr_i curr_i matrix of
                Just idx -> 
                    if (idx == (length $ head $ getM matrix)-1)
                    then error "_gaussianElimSteps: inconsistent system of equations"
                    else (show ("swapcol", curr_i, idx), matrix):_gaussianElimSteps curr_i
                                                                                    last_i
                                                                                    (Just $ swapCols idx curr_i matrix)
                Nothing -> (show ("del row", curr_i, -1), matrix):_gaussianElimSteps curr_i last_i (Just $ deleteRow curr_i matrix)

findIdxOfRowWithColNonzero :: Int -> Int -> EqSysMatrix -> Maybe Int
--findIdxOfRowWithColNonzero from col m | myptrace ("findIdxOfRowWithColNonzero " ++ show from ++ " " ++ show col ++ " " ++ show m) False = undefined
findIdxOfRowWithColNonzero from col m =
    fmap (+from) $ L.findIndex ((/= 0) . (!! col)) $ drop from $ getM m

findIdxOfColWithRowNonzero :: Int -> Int -> EqSysMatrix -> Maybe Int
findIdxOfColWithRowNonzero from row m =
    fmap (+from) $ L.findIndex (/=0) $ drop from $ (getM m !! row)

zeroColUsingRow :: Int -> Int -> EqSysMatrix -> EqSysMatrix
zeroColUsingRow col_i row_i (Matrix { getM=m, getColIds=col_ids, getEqPos=eq_pos }) =
    let row = m !! row_i
        this_val = row !! col_i
        scalars = map (\r -> (r !! col_i) / this_val) m
        new_m = map (mapRows row this_val row_i) $ enumerate $ zip scalars m
     in (Matrix { getM=new_m, getColIds=col_ids, getEqPos=eq_pos })
    where
        mapRows row this_val row_i (i, (s, r)) =
            if i == row_i
            then map (/this_val) r
            else zipWith (+) r $ map (*(-s)) row

deleteRow :: Int -> EqSysMatrix -> EqSysMatrix
deleteRow i Matrix { getM = m, getColIds = col_ids, getEqPos = eq_pos } =
    Matrix { getM = take i m ++ drop (i+1) m, getColIds = col_ids, getEqPos = eq_pos}

-- getSolutionsFromSolved :: EqSysMatrix -> Solutions
getSolutionsFromSolved matrix@(Matrix { getM = m, getColIds = col_ids, getEqPos = eq_pos }) =
    let bound_var_count = 1 + (fromJust $ L.findIndex (/=0) $ last $ m)
        free_var_count = length col_ids - bound_var_count
        padding = replicate free_var_count 0
        matrix' = moveEqSign bound_var_count matrix
        base_vals = extractBaseVals free_var_count matrix'
        free_vars = extractFreeVars bound_var_count matrix'
     in Solutions {getBaseVals = base_vals, getFreeVars = free_vars}

extractBaseVals pad matrix =
    let vals = map (last) (getM matrix) ++ replicate pad 0
        sorted = map snd $ L.sort $ zip (getColIds matrix) vals
     in sorted

extractFreeVars bound_var_count matrix = 
    let col_ids = getColIds matrix
        free_var_ids = drop bound_var_count $ col_ids
        free_var_cols = map (\i -> (getColumn (fromJust $ L.elemIndex i col_ids) $ getM matrix)) free_var_ids
        free_var_cols' = map (++ replicate (length free_var_ids) 0) free_var_cols
        sorted_with_ids = map (\col -> L.sort $ zip col_ids col) free_var_cols'
        zipped = zip free_var_ids sorted_with_ids
        final = map (\(free_i, xs) -> (free_i, map (\(i,v) -> if free_i == i then 1 else v) xs)) zipped
     in final

moveEqSign :: Int -> EqSysMatrix -> EqSysMatrix 
moveEqSign new_pos (Matrix { getM = m, getColIds = col_ids, getEqPos = eq_pos }) =
    let neg_from = min new_pos eq_pos
        neg_to = max new_pos eq_pos
        new_m = map (map (negBetweenIndices neg_from neg_to) . enumerate) m
     in Matrix { getM = new_m, getColIds = col_ids, getEqPos = new_pos }
    where negBetweenIndices from to (i, val) = if i >= from && i < to
                                                 then negate val
                                                 else val

enumerate :: [a] -> [(Int, a)]
enumerate xs = aux 0 xs
    where aux i [] = []
          aux i (x:xs) = (i,x):aux (i+1) xs

solveSystem :: EqSysMatrix -> Maybe Solutions
solveSystem = fmap getSolutionsFromSolved . gaussianElim

solutionsToList :: Solutions -> [[MatrixVal]]
solutionsToList _ = []

-- swaps
swapAtIndices :: Int -> Int -> [a] -> [a]
swapAtIndices i j xs
    | i > j = swapAtIndices j i xs
    | i == j = xs
swapAtIndices 0 j (xi:xs) =
    let (before, xj:after) = splitAt (j-1) xs
     in xj:before ++ xi:after
swapAtIndices i j (x:xs) =
    x:(swapAtIndices (i-1) (j-1) xs)

swapRows :: Int -> Int -> EqSysMatrix -> EqSysMatrix
swapRows i j Matrix { getM = m, getColIds = col_ids, getEqPos = eq_pos } =
    Matrix { getM = swapAtIndices i j m, getColIds = col_ids, getEqPos = eq_pos}

swapCols :: Int -> Int -> EqSysMatrix -> EqSysMatrix
swapCols i j Matrix { getM = m, getColIds = col_ids, getEqPos = eq_pos } =
    Matrix { getM = map (swapAtIndices i j) m, getColIds = swapAtIndices i j col_ids, getEqPos = eq_pos}

-- row ops
addRowToRow :: Int -> Int -> EqSysMatrix -> EqSysMatrix
addRowToRow addend_i acc_i  m = m

mulRowByScalar :: Int -> MatrixVal -> EqSysMatrix -> EqSysMatrix
mulRowByScalar row_i scalar m = m
