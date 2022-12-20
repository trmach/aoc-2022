import System.IO
import Data.List

filename :: String
filename = "Day6.txt"

example1 :: String
example1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
example2 :: String
example2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
example3 :: String
example3 = "nppdvjthqldpwncqszvftbrmjlhg"
example4 :: String
example4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
example5 :: String
example5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

findMarker :: String -> Int
findMarker [] = undefined -- we should get an error if for some reason we reach the end of a list
findMarker l | length (nub $ take 4 l) < 4 = 1 + findMarker (drop 1 l)
             | otherwise = 4

findMessage :: String -> Int
findMessage [] = undefined -- we should get an error if for some reason we reach the end of a list
findMessage l | length (nub $ take 14 l) < 14 = 1 + findMessage (drop 1 l)
             | otherwise = 14

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print $ findMarker contents
    print $ findMessage contents
    hClose handle