import LinearSolver
import qualified Data.List as L
import Data.Bits
import Data.Maybe (isJust, fromJust)
import Data.Ratio
import Text.Printf
import Debug.Trace

debug = flip trace

newtype State = State {getState :: Int} deriving (Eq, Ord)
initState :: State
initState = State {getState = 0}

newtype Button = Button {getChange :: Int}

newtype Joltages = Joltages {getJoltages :: [Int]} deriving (Eq, Ord)
initJoltages :: Int -> Joltages
initJoltages n = Joltages {getJoltages = replicate n 0}

type Machine = (State, [Button], Joltages)

instance Show State where
    show :: State -> String
    show g = '[' : (map d2c $ digits 2 $ getState g) ++ "]"
        where d2c :: (Show a, Integral a) => a -> Char
              d2c 1 = '#'
              d2c 0 = '.'
              d2c d = error $ "invalid binary digit" ++ (show d)

digits :: Integral a => a -> a -> [a]
digits _ 0 = []
digits b n = let (n', d) = divMod n b
              in d:digits b n'

instance Read State where
    readsPrec _ ('[':input) =
        let (goal_str, rest) = span (`elem` ".#") input
            state = str2num goal_str
         in if head rest /= ']'
            then []
            else [(State {getState=state}, tail rest)]
        where c2d '#' = 1
              c2d '.' = 0
              c2d _ = error "invalid digit char"
              str2num = snd
                       . foldl (\(mult, total) d -> (mult*2, total+d*mult)) (1, 0)
                      . map c2d
    readsPrec _ _ = []


instance Show Button where
    show b = '(' : ( init $ tail $ show $ L.unfoldr unfolder (0, getChange b)) ++ ")"
        where unfolder (_, 0)= Nothing
              unfolder (pow, a) = let (d, m) = a `divMod` 2
                                   in if m == 1
                                      then Just (pow, (pow+1, d))
                                      else unfolder (pow+1, d)

instance Read Button where
    readsPrec prec ('(':input) =
        let (button_str, rest) = span (`elem` "0123456789,)") input
         in case readsPrec prec ('[':(init button_str)++"]") :: [([Int],String)] of
                [] -> []
                [(list, "")] -> [(Button {getChange = sum $ map (2^) list}, rest)]
                _ -> []
    readsPrec _ _ = []

instance Show Joltages where
    show joltages = '{':(init $ tail $ show $ getJoltages joltages) ++ "}"
instance Read Joltages where
    readsPrec prec ('{':input) = 
        case span (/= '}') input of
            (nums, '}':rest) ->
                case readsPrec prec ('[':nums++"]") of
                    [(list :: [Int], "")] -> [(Joltages { getJoltages = list }, rest)]
                    _ -> []
            _ -> []
    readsPrec _ _ = []

parse :: String -> [Machine]
parse = map parseMachine . lines

parseMachine :: String -> Machine
parseMachine str = let (goalStr:rest) = words str
                       goal = read goalStr
                       buttons :: [Button] = map read $ init rest
                       joltages :: Joltages = read $ last rest
                    in (goal, buttons, joltages)

eqSysMatrixFromMachine :: Machine -> EqSysMatrix
eqSysMatrixFromMachine (_, buttons, joltages) = 
    let eq_count = length $ getJoltages joltages
        matrix = map (\(i,v) -> (bs2eqFor buttons i) ++ [fromIntegral v]) $ enumerate $ getJoltages joltages
        button_count = length buttons
        button_ids = [0..button_count-1]
     in Matrix { getM = matrix, getColIds = button_ids, getEqPos=button_count }
    where bs2eqFor :: [Button] -> Int -> [MatrixVal]
          bs2eqFor [] i = []
          bs2eqFor (b:bs) i = (if 0 /= ((getChange b) .&. (shiftL 1 i))
                              then 1
                              else 0):bs2eqFor bs i


calcSolutions file = map (solveSystem . eqSysMatrixFromMachine) . parse <$> readFile file

--transformToIntegral :: Solutions -> Solutions

transformToIntegral Solutions { getBaseVals = base_vals, getFreeVars = free_vars }
    | myptrace "transformToIntegral" False = undefined
    | (all $ (==1) . denominator) base_vals =
        let lcms = map (fromIntegral . foldl (\acc v -> lcm acc $ denominator v) 1 . snd) free_vars
            zipped = zip lcms free_vars
            integral_free_vars = map (\(_lcm, (idx, vals)) -> (idx, scalar_mul vals _lcm)) zipped
         in Solutions { getBaseVals = base_vals, getFreeVars = integral_free_vars }
    | otherwise =
        let lcms = map (fromIntegral . foldl (\acc v -> lcm acc $ denominator v) 1 . snd) free_vars
            zipped = zip lcms free_vars
            integral_free_vars = map (\(_lcm, (idx, vals)) -> (idx, scalar_mul vals _lcm)) zipped
            options = map aux $ zipped
            lincombs = foldl1 (\acc v -> map vector_sum v <*> acc) options
            lincombs' = map (vector_sum base_vals) lincombs
            coeffs = map (1:) $ choices (map (\a -> [-2*a..2*a]) lcms)
            vecs = base_vals:(map snd free_vars)
            lincombs'' = map (linear_comb vecs) coeffs
            integral_base_vals = head 
                               $ filter (all $ (==1) . denominator) lincombs'
            integral_base_vals'' = head 
                               $ filter (all $ (>=0))
                               $ filter (all $ (==1) . denominator) lincombs''
            in Solutions { getBaseVals = integral_base_vals'', getFreeVars = integral_free_vars }
    where aux (0, (idx, vals)) = []
          aux (_lcm, (idx, vals)) = map (scalar_mul vals) [0.._lcm-1]

--This is terrible:
--1) hurr durr bruteforce
--2) assumes a nonnegative integral solution exists
--   (doesn't halt otherwise)
--because I'm bad at linalg and can't figure out a general solution

--transformToNonnegative :: Solutions -> Maybe Solutions
transformToNonnegative orig@(Solutions { getBaseVals = base_vals, getFreeVars = free_vars })
    | myptrace ("transformToNonnegative: " ++ show (length base_vals) ++ " " ++ show (length free_vars)) False = undefined
    | length free_vars == 0 = if (all (>=0) base_vals) then Just orig else Nothing
    | otherwise = 
        let 
            vecs = base_vals:(map snd free_vars)
            dims = length free_vars
            coeffs = coeffGen dims
            lincombs = map (linear_comb vecs . (1:)) coeffs
            valid = filter (all (>=0)) lincombs
            new_base = head valid
         in Just Solutions { getBaseVals = new_base, getFreeVars = free_vars }

transformToMinimal orig@(Solutions { getBaseVals = base_vals, getFreeVars = free_vars }) =
    let a = 0
     in a

printSolutions :: Solutions -> IO ()
printSolutions = putStrLn . showSolutions "% 6.2f"

showSolutions :: String -> Solutions -> String
showSolutions col_fmt Solutions { getBaseVals = base_vals, getFreeVars = free_vars } =
    let base_val_str = unwords $ map fmt base_vals
        free_var_strs = map (\a -> (printf "%4s: " ('x':show (fst a))) ++ (unwords $ (map fmt) $ snd a)) free_vars
     in unlines (("base: " ++ base_val_str):free_var_strs)
    where fmt :: Rational -> String
          fmt v = let val :: Float = fromRational v
                   in printf col_fmt val

scalar_mul :: [MatrixVal] -> MatrixVal -> [MatrixVal]
scalar_mul vec scalar = map (*scalar) vec
vector_sum vec1 vec2
    | length vec1 == length vec2 = zipWith (+) vec1 vec2
    | otherwise = error "vector_sum: cannot add vectors of different lengths"

linear_comb vecs coeffs = foldl1 (vector_sum) $ zipWith (scalar_mul) vecs coeffs

coeffGen 0 = []
coeffGen len = concat $ map choices $ aux len 0
    where aux len 0 = replicate len [0]:aux len 1
          aux len n = map (wololo len n) [0..len-1] ++ aux len (n+1)

wololo len n i = replicate i [0..n-1] ++  [n]:replicate (len-i-1) [0..n]

choices :: [[a]] -> [[a]]
choices [] = []
choices ([]:xs) = choices xs
choices ([x]) = map (L.singleton) x
choices (xs:rest) = map (:) xs <*> choices rest


solutionSets file = do
    solutions_maybe_invalid <- calcSolutions file
    let (justs, nothings) = L.partition (\s -> isJust s) solutions_maybe_invalid
    putStrLn $ "Solutions found for " ++ show (length justs) ++ " of " ++ show (length justs + length nothings) ++ " problems"
    let solutions_maybe = map (fmap transformToIntegral . transformToNonnegative . fromJust) justs
    let (transformed, failed) = L.partition (\s -> isJust s) solutions_maybe
    putStrLn $ "Successfully transformed " ++ show (length transformed) ++ " solutions"
    --let solutions = map fromJust justs
    mypprint $ head transformed
    let (pos, neg) = L.partition (\s -> all (>=0) $ getBaseVals s) $ map fromJust transformed
    putStrLn $ "After transforming to valid:"
    putStrLn $ "Of those, " ++ show (length pos) ++ " have valid (nonneg) basevals and " ++ show (length neg) ++ " don't"
    let (posint, posfrac) = L.partition (\s -> all ((==1) . denominator) $ getBaseVals s) pos
    let (negint, negfrac) = L.partition (\s -> all ((==1) . denominator) $ getBaseVals s) neg

    putStrLn $ "Of the positive solutions, " ++ show (length posint) ++
               " are integral and " ++ show (length posfrac) ++ " are fractional"
    putStrLn $ "Of the negative solutions, " ++ show (length negint) ++
               " are integral and " ++ show (length negfrac) ++ " are fractional"
    printSolutions $ head negint
    print $ L.findIndex (== (Just $ head negint)) transformed


    let (allbound, somefree) = L.partition (\s -> null $ getFreeVars s) $ map fromJust justs
    putStrLn $ "Of all solutions, " ++ show (length somefree) ++
               " have some free variables and " ++ show (length allbound) ++ " have none"

    let (boundpos, boundneg) = L.partition (\s -> all (>=0) $ getBaseVals s) allbound
    let (boundposint, boundposfrac) = L.partition (\s -> all ((==1) . denominator) $ getBaseVals s) boundpos

    putStrLn $ "Of those with none, " ++ show (length boundposint) ++
               " are valid, " ++ show (length boundposfrac) ++
               " are nonnegative but fractional, and " ++ show (length boundneg) ++
               " are negative"

    let (_, posintfree) = L.partition (\s -> null $ getFreeVars s) $ posint
    putStrLn $ "What follows is some of the solutions:"
    putStrLn $ "1. positive, nonfractional, free (3 examples)"
    let aaa = take 1 $ filter (\s -> (length $ getBaseVals s) <= 5) posintfree
    let bbb = take 2 $ filter (\s -> (length $ getBaseVals s) <= 7 && (length $ getFreeVars s) > 1)  posintfree
    mypprint $ aaa ++ bbb
    putStrLn ""

    putStrLn $ "1. negative, nonfractional, free (3 examples)"
    mypprint $ take 3 $ filter (\s -> (length $ getBaseVals s) <= 5 && (not $ null $ getFreeVars s)) negint
    putStrLn ""

    putStrLn $ "1. positive, fractional (all)"
    mypprint $ map (\s -> (s, transformToIntegral s)) posfrac
    putStrLn ""

    putStrLn $ "1. negative, fractional (all)"
    mypprint $ init $ map (\s -> (s, transformToIntegral s)) negfrac
    putStrLn ""


example_solutions1 =  Solutions
        { getBaseVals =
            [
                ( -356 ) % 3, 138 % 1, 227 % 3,
                ( -139 ) % 3,
                ( -188 ) % 3, 37 % 1,
                ( -2 ) % 3, 70 % 1, 325 % 3, 0 % 1, 0 % 1
            ], getFreeVars =
            [
                ( 9,
                    [ 14 % 3,
                        ( -5 ) % 1,
                        ( -8 ) % 3, 7 % 3, 8 % 3,
                        ( -1 ) % 1,
                        ( -1 ) % 3,
                        ( -2 ) % 1,
                        ( -10 ) % 3, 1 % 1, 0 % 1
                    ]
                ),
                ( 10,
                    [ 7 % 3,
                        ( -2 ) % 1,
                        ( -4 ) % 3, 2 % 3, 4 % 3,
                        ( -1 ) % 1, 1 % 3,
                        ( -1 ) % 1,
                        ( -5 ) % 3, 0 % 1, 1 % 1
                    ]
                )
            ]
        }
example_solutions2 = Solutions
    { getBaseVals =
        [ 8 % 1, 17 % 1, 5 % 1, 0 % 1, 2 % 1, 112 % 1, 11 % 1
        ], getFreeVars = []
    }

solution127int = Solutions
    { getBaseVals =
        [
            ( -1 ) % 1, 37 % 1, 43 % 1, 15 % 1,
            ( -18 ) % 1, 16 % 1, 132 % 1, 22 % 1,
            ( -2 ) % 1, 30 % 1, 4 % 1, 0 % 1
        ], getFreeVars =
        [
            ( 10,
                [ 1 % 1,
                    ( -7 ) % 1,
                    ( -19 ) % 1,
                    ( -1 ) % 1, 11 % 1,
                    ( -10 ) % 1, 8 % 1,
                    ( -6 ) % 1, 0 % 1, 3 % 1, 14 % 1, 0 % 1
                ]
            ),
            ( 11,
                [ 3 % 1,
                    ( -7 ) % 1,
                    ( -15 ) % 1,
                    ( -3 ) % 1, 5 % 1,
                    ( -2 ) % 1, 10 % 1,
                    ( -4 ) % 1, 14 % 1,
                    ( -19 ) % 1, 0 % 1, 14 % 1
                ]
            )
        ]
    }

solution72 = Solutions
        { getBaseVals =
            [
                ( -85 ) % 3, 35 % 3, 69 % 1, 30 % 1, 104 % 1, 85 % 3,
                ( -133 ) % 3,
                ( -71 ) % 3, 61 % 3,
                ( -334 ) % 3, 0 % 1, 0 % 1, 0 % 1
            ], getFreeVars =
            [
                ( 10,
                    [ 0 % 1, 0 % 1,
                        ( -1 ) % 1, 0 % 1,
                        ( -1 ) % 1, 0 % 1, 1 % 1, 0 % 1, 0 % 1, 1 % 1, 1 % 1, 0 % 1, 0 % 1
                    ]
                ),
                ( 11,
                    [ 5 % 3,
                        ( -1 ) % 3,
                        ( -2 ) % 1,
                        ( -1 ) % 1,
                        ( -3 ) % 1,
                        ( -2 ) % 3, 5 % 3, 4 % 3,
                        ( -2 ) % 3, 11 % 3, 0 % 1, 1 % 1, 0 % 1
                    ]
                ),
                ( 12,
                    [ 5 % 3,
                        ( -1 ) % 3,
                        ( -2 ) % 1,
                        ( -1 ) % 1,
                        ( -3 ) % 1,
                        ( -2 ) % 3, 2 % 3, 4 % 3,
                        ( -2 ) % 3, 11 % 3, 0 % 1, 0 % 1, 1 % 1
                    ]
                )
            ]
        }

bbb n = transformToIntegral . fromJust . transformToNonnegative . fromJust . head . drop n 
test = do
    solutions <- calcSolutions "data.txt"
    let aaa = map (\n -> (n `debug` (show n), solutions !! n, bbb n solutions)) [0..177]
    mypprint aaa
