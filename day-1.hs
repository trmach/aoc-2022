import MP4
import System.IO
import Control.Monad
import Data.List.Split
import Data.List

filename = "day-1.txt"

hell :: [[Int]] -> [[String]] -> [[Int]] -- hell
hell _ [] = []
hell c (x:xs) = (map (read :: String -> Int) x) : (hell c xs)

main :: IO ()
main = do
    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = (reverse . sort) $ (map sum (hell [[]] (splitWhen (=="") (lines contents))))
    print $ head l -- Part 1
    print $ (sum . take 3) l -- Part 2
    hClose handle