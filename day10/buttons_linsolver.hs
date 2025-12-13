import LinearSolver
import qualified Data.List as L
import Data.Bits
import Data.Maybe (isJust, fromJust)
import Data.Ratio

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


solutionSets file = do
    solutions <- calcSolutions file
    let (justs, nothings) = L.partition (\s -> isJust s) solutions
    putStrLn $ "Solutions found for " ++ show (length justs) ++ " of " ++ show (length justs + length nothings) ++ " problems"
    let (pos, neg) = L.partition (\s -> all (>=0) $ getBaseVals s) $ map fromJust justs
    putStrLn $ "Of those, " ++ show (length pos) ++ " have valid (nonneg) basevals and " ++ show (length neg) ++ " don't"
    let (posint, posfrac) = L.partition (\s -> all ((==1) . denominator) $ getBaseVals s) pos
    let (negint, negfrac) = L.partition (\s -> all ((==1) . denominator) $ getBaseVals s) neg

    putStrLn $ "Of the positive solutions, " ++ show (length posint) ++
               " are integral and " ++ show (length posfrac) ++ " are fractional"
    putStrLn $ "Of the negative solutions, " ++ show (length negint) ++
               " are integral and " ++ show (length negfrac) ++ " are fractional"

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
    mypprint $ take 1 $ filter (\s -> (length $ getBaseVals s) <= 5) posintfree
    mypprint $ take 2 $ filter (\s -> (length $ getBaseVals s) <= 7 && (length $ getFreeVars s) > 1)  posintfree
    putStrLn ""

    putStrLn $ "1. negative, nonfractional, free (3 examples)"
    mypprint $ take 3 $ filter (\s -> (length $ getBaseVals s) <= 5 && (not $ null $ getFreeVars s)) negint
    putStrLn ""

    putStrLn $ "1. positive, fractional (all)"
    mypprint $ posfrac
    putStrLn ""

    putStrLn $ "1. negative, fractional (all)"
    mypprint $ negfrac
    putStrLn ""


