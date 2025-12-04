module Grid ( Grid(..)
            , fromList
            , toList
            , gridWindowsFill3x3
            , gridAt
            ) where

import Data.List(unfoldr)

data Grid a = Grid Int Int [a]

instance (Show a) => Show (Grid a) where
    show g = showDims g ++ "\n" ++ rows_string g
        where rows_string = init -- drops the last newline
                          . unlines
                          . map show
                          . getRows

instance Functor Grid where
    fmap fn (Grid w h l) = (Grid w h (map fn l))

instance Foldable Grid where
    foldr fn acc (Grid _ _ l) = foldr fn acc l

getRows :: Grid a -> [[a]]
getRows (Grid w h l) = unfoldr aux l
    where aux :: [a] -> Maybe ([a], [a])
          aux [] = Nothing
          aux l 
            | length l < w = error "getRows: length of l was lt w - this should never've happened"
            | otherwise = Just (splitAt w l)
showDims :: Grid a -> String
showDims (Grid w h _) = "Grid w=" ++ (show w) ++ " h=" ++ (show h)

fromList :: Int -> Int -> [a] -> Grid a
fromList w h l
    | length l /= w*h = error error_length_string
    | otherwise = Grid w h l
    where error_length_string = showDims (Grid w h []) ++
                                " can only be constructed from a list of length " ++ (show (w*h)) ++
                                "; got: " ++ (show $ length l)

toList :: Grid a -> [a]
toList (Grid _ _ l) = l

gridAt :: Int -> Int -> Grid a -> Maybe a
gridAt x y (Grid w h l) 
    | x >= 0 && x < w && y >= 0 && y < h = Just (l !! (y*w+x))
    | otherwise = Nothing

listWindows :: Int -> [a] -> [[a]]
listWindows n [] = []
listWindows n list@(_:tail)
    | length list < n = []
    | otherwise = (take n list):(listWindows n tail)

listWindowsFill :: Int -> a -> [a] -> [[a]]
listWindowsFill n fill list = listWindows n (edge ++ list ++ edge)
    where edge = replicate (n`div`2) fill

gridWindowsFill3x3 :: a -> Grid a -> [Grid a]
gridWindowsFill3x3 fill grid@(Grid w h l) = windowList
    where rowWins = map (listWindowsFill 3 fill) $ getRows grid
          row_empty = replicate w (replicate 3 fill)
          rowWinsFilled = row_empty:rowWins ++ [row_empty]
          rowWinsFilled' = concat rowWinsFilled
          windowTuples = zip3 rowWinsFilled' (drop w rowWinsFilled') (drop (2*w) rowWinsFilled')
          windowList = map (\(a,b,c) -> fromList 3 3 (a++b++c)) windowTuples

gridIntsSquare :: Int -> Grid Int
gridIntsSquare n = fromList n n [1..n*n]
gridInts3 = gridIntsSquare 3
gridInts5 = gridIntsSquare 5
