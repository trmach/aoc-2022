import System.IO
import Data.List hiding (intersect)
import Data.Maybe

import Data.Set hiding (splitAt, map)

filename :: String
filename = "Day3.txt"

letters :: [Char]
letters = '$' : ['a'..'z'] ++ ['A'..'Z']

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

map' :: (a -> b) -> (a,a) -> (b,b)
map' f (a1, a2) = (f a1, f a2)

map3 :: (a -> b) -> (a,a,a) -> (b,b,b)
map3 f (a1, a2, a3) = (f a1, f a2, f a3)

construct3Tuples :: [a] -> [(a,a,a)]
construct3Tuples [] = []
construct3Tuples [_] = []
construct3Tuples [_,_] = []
construct3Tuples (x:y:z:xs) = (x,y,z) : construct3Tuples xs

intersect :: Ord a => (Set a, Set a) -> Set a
intersect (x,y) = x `intersection` y

intersect3 :: Ord a => (Set a, Set a, Set a) -> Set a
intersect3 (x,y,z) = x `intersection` y `intersection` z

priorities :: Char -> Int
priorities x = fromMaybe 0 (elemIndex x letters)

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = lines contents
    print $ sum $ map (priorities . findMin . intersect . map' fromList . halve) l -- findMin bc how else do you get the element
    print $ sum $ map (priorities . findMin . intersect3 . map3 fromList) $ construct3Tuples l -- hell
    hClose handle