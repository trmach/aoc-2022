import System.IO
import Data.List.Split

filename :: String
filename = "Day4.txt"

isWithin :: [[Int]] -> Bool
isWithin [] = False
isWithin [[]] = False
isWithin [_:_] = False
isWithin (x:y:_) = (head x <= head y && last x >= last y) || (head y <= head x && last y >= last x)

includeOverlap :: [[Int]] -> Bool
includeOverlap [] = False
includeOverlap [[]] = False
includeOverlap [_:_] = False
includeOverlap l@(x:y:_) = isWithin l || (head x <= last y && last x >= head y) || (head y <= last x && last y >= head x)

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = lines contents
    -- If there's a place you gotta go, I'm the one you need to know...
    print $ length $ filter (==True) $ map (isWithin . map (map (read :: String -> Int) . splitOn "-") . splitOn ",") l
    -- If there's a place you gotta get, I can get you there I bet...
    print $ length $ filter (==True) $ map (includeOverlap . map (map (read :: String -> Int) . splitOn "-") . splitOn ",") l
    hClose handle