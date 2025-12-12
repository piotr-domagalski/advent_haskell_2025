import qualified Data.List as L
import System.Environment

type PresentShape = Int
data Present = Present { i :: Int, shape :: PresentShape }
data TreeSpec = TreeSpec { w :: Int, h :: Int, counts :: [Int] }

instance Show TreeSpec where
    show TreeSpec { w, h, counts } =
        show w ++ "x" ++ show h ++ ": " ++
        (unwords $ map show $ counts)

instance Read TreeSpec where
    readsPrec _ str = 
        let (wstr, 'x':rest1) = span (/='x') str
            (hstr, ':':' ':countstr) = span (/=':') rest1
            counts = map read $ words countstr

         in [(TreeSpec { w=read wstr, h=read hstr, counts=counts }, "")]

area :: TreeSpec -> Int
area TreeSpec { w, h } = w * h

instance Show Present where
    show Present { i, shape } = show i ++ ": " ++ show shape
    {-
        unlines [ show i ++ ":"
                , show shape
                , show shape
                , show shape
                ]
                -}

instance Read Present where
    readsPrec _ str = let (i, ':':shape) = span (/=':') str
                          count = length $ filter (=='#') shape
                       in [(Present { i=read i, shape=count }, "")]



--parse :: String -> ([Present], [TreeSpec])
parse str = let specs = L.groupBy (\a b -> L.null a == L.null b) $ lines str
                presents = filter (/=[""]) $ init specs
                trees = last specs
             in (map (read . unlines) presents :: [Present], map read $ trees :: [TreeSpec])

main = do
    file:_ <- getArgs
    main' file

main' file = do
    contents <- readFile file
    let (presents, trees) = parse contents
        areas = map (\tree -> (area tree, sum $ zipWith (\p c -> shape p * c) presents (counts tree))) trees
        maybe_possible = filter (\(at,ap) -> at > ap) areas
        maybe_possible_count = length maybe_possible
        avg_difference = (fromIntegral $ sum $ map (\(at,ap) -> at - ap) $ maybe_possible) / (fromIntegral maybe_possible_count)
        areas_bb = map (\tree -> (area tree, (9*) $ sum $ counts tree)) trees
        maybe_possible_bb = filter (\(at,ap) -> at > ap) areas_bb
        maybe_possible_bb_count = length maybe_possible_bb
        avg_difference_bb = (fromIntegral $ sum $ map (\(at,ap) -> at - ap) $ maybe_possible_bb) / (fromIntegral maybe_possible_bb_count)
    putStrLn $ "Areas (tree, required presents total):      " ++ show areas
    putStrLn $ "Count (where a_tree > a_required_presents): " ++ show maybe_possible_count
    putStrLn $ "Of those, avg difference (tree-presents):   " ++ show avg_difference
    putStrLn $ ""
    putStrLn $ "Count by BB (a_tree > 9*present_count):     " ++ show maybe_possible_bb_count
    putStrLn $ "Of those, avg difference by BB:             " ++ show avg_difference_bb
    putStrLn $ ""
    putStrLn $ "This shouldn't have worked, but the answer is: " ++ show maybe_possible_count
