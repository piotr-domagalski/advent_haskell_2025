import System.Environment

-- consts
dialNumCount = 100
dialInitPos = 50

data Counter = Counter {ce :: Int, cc :: Int, pos :: Int, rot :: Int} deriving Show
initCounter = Counter {cc=0, ce=0, pos=dialInitPos, rot=0}

type ResultFormat = [Counter] -> String
type Folder = Counter -> Int -> Counter

main :: IO ()
main = getArgs >>= main' normal . head

main' :: ([Counter] -> String) -> String -> IO ()
main' format file = format . solve folder_cross . parse <$> readFile file >>= putStrLn

normal :: ResultFormat
normal = show . (\c -> (ce c, cc c)) . last

debug :: ResultFormat
debug = unlines . map show

parse :: String -> [Int]
parse = map (\(x:xs) -> (if x == 'L' then negate else id) $ read xs) . lines

solve :: Folder -> [Int] -> [Counter]
solve folder = scanl folder initCounter

folder_cross :: Folder
folder_cross Counter {cc=count_cross, ce=count_end, pos=pos} rot =
         let fullturns = abs $ rot `quot` dialNumCount
             rot' = rot `rem` dialNumCount
             newpos = pos + rot'
             crossed = if pos /= 0 && (newpos > dialNumCount || newpos < 0) then 1 else 0
             newpos' = newpos `mod` dialNumCount
             ended = if newpos' == 0 then 1 else 0
             in
             Counter {cc=count_cross+fullturns+crossed+ended, ce=count_end+ended, pos=newpos', rot=rot}
