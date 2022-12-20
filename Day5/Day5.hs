import System.IO
import Data.List.Split
import Data.Char (isSpace)

filename :: String
filename = "Day5.txt"

constructStack :: Int -> [String] -> [Char]
constructStack _ [] = []
constructStack i (x:xs) = x !! (i*4 - 3) : constructStack i xs -- index in list from number is i*4 - 3

move :: Int -> Int -> Int -> [String] -> [String] -- maybe a better way to do this...
move q l1 l2 l = take (l2-1) with1 ++ newStack2 : drop l2 with1 -- finally insert new stack 2       <-----\
    where stack1 = l !! (l1-1)                              -- get the stacks we want to alter            \
          stack2 = l !! (l2-1)                              -- get the stacks we want to alter            \
          newStack1 = drop q stack1                         -- stack having elements removed              |
          from = take q stack1                              -- elements removed *from* stack 1            |
          newStack2 = reverse from ++ stack2                -- stack 2 has elements reversed and added    |
          with1 = take (l1-1) l ++ newStack1 : drop l1 l    -- insert new stack 1 into list >------------/

-- use the same function but without reverse !!!!!

move' :: Int -> Int -> Int -> [String] -> [String]
move' q l1 l2 l = take (l2-1) with1 ++ newStack2 : drop l2 with1
    where stack1 = l !! (l1-1)
          stack2 = l !! (l2-1)
          newStack1 = drop q stack1
          from = take q stack1
          newStack2 = from ++ stack2
          with1 = take (l1-1) l ++ newStack1 : drop l1 l


-- accepts single command, one of the move functions, and our list of stacks, returns updated stack by applying f
execMove :: String -> (Int -> Int -> Int -> [String] -> [String]) -> [String] -> [String]
execMove stmt f = f (read $ cmd !! 1) (read $ cmd !! 3) (read $ cmd !! 5)
    where cmd = words stmt

execAll :: [String] -> (Int -> Int -> Int -> [String] -> [String]) -> [String] -> [String]
execAll [] _ stacks = stacks
execAll stmts@(x:xs) f stacks = do 
    let newStacks = execMove x f stacks
    execAll xs f newStacks

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = splitWhen (=="") $ lines contents
        nCols = (read :: String -> Int) $ last $ words $ last (head l)
        boxes = [ dropWhile isSpace $ constructStack i (head l) | i <- [1..nCols] ]
        moves = last l

    print boxes
    print $ execAll moves move boxes
    print $ map head (execAll moves move boxes)
    print $ execAll moves move' boxes
    print $ map head (execAll moves move' boxes)
    hClose handle