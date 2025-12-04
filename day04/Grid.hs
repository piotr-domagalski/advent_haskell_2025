module Grid ( fromList
            , Grid
            ) where

import Data.List(unfoldr)

data Grid a = Grid Int Int [a]

instance (Show a) => Show (Grid a) where
    show g = showDims g ++ "\n" ++ rows_string g
        where rows_string = init -- drops the last newline
                          . unlines
                          . map show
                          . getRows

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
