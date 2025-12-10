import qualified Data.List as L
import Data.Bits
import System.Environment

newtype State = State {getState :: Int} deriving Eq
initState :: State
initState = State {getState = 0}

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

newtype Button = Button {getChange :: Int}

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


parse :: String -> [(State, [Button])]
parse = map parseMachine . lines

parseMachine :: String -> (State, [Button])
parseMachine str = let (goalStr:rest) = words str
                       goal = read goalStr
                       buttons :: [Button] = map read $ init rest
                    in (goal, buttons)

press :: Button -> State -> State
press b s = State {getState = getState s `xor` getChange b}

solve :: [(State, [Button])] -> [[(Int, [State])]]
solve machines = map (\(goal, bs) -> solve' 0 goal [initState] (map press bs)) machines
solve' :: Int -> State -> [State] -> [State -> State] -> [(Int, [State])]
solve' i goal states presses = let states' = presses <*> states
                                in if goal `elem` states' 
                                   then [(i, states), (i+1, states')]
                                   else (i, states):solve' (i+1) goal states' presses

main :: IO ()
main = do
    file:_ <- getArgs
    main' file

main' :: String -> IO ()
main' file = do
    contents <- readFile file
    let parsed = parse contents
        solved1 = solve parsed
        counts1 = map (fst . last) solved1
    putStrLn $ (concat $ L.intersperse "+" $ map show counts1)
    putStrLn $ "Task 1: " ++
               (show $ sum $ counts1)
