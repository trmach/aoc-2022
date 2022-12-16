{-# LANGUAGE InstanceSigs #-}
import System.IO

filename :: String
filename = "Day2.txt"

data Verdict = Win | Loss | Tie | Mmm
data Shape = Rock | Paper | Scissors

instance Show Verdict where
    show :: Verdict -> String
    show Win = "Win"
    show Loss = "Loss"
    show Tie = "Tie"
    show Mmm = "Mmm"

instance Eq Verdict where
    (==) :: Verdict -> Verdict -> Bool
    Win == Win = True
    Win == _ = False
    Loss == Loss = True
    Loss == _ = False
    Tie == Tie = True
    Tie == _ = False
    Mmm == Mmm = True
    Mmm == _ = False

instance Show Shape where
    show :: Shape -> String
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

instance Eq Shape where
    (==) :: Shape -> Shape -> Bool
    Rock == Rock = True
    Rock == _ = False
    Paper == Paper = True
    Paper == _ = False
    Scissors == Scissors = True
    Scissors == _ = False

(===) :: Shape -> Shape -> Verdict
Rock === Rock = Tie         -- Rock/Rock
Paper === Rock = Loss       -- Paper/Rock
Scissors === Rock = Win     -- Scissors/Rock
Rock === Paper = Win        -- Rock/Paper
Paper === Paper = Tie       -- Paper/Paper
Scissors === Paper = Loss   -- Scissors/Paper
Rock === Scissors = Loss    -- Rock/Scissors
Paper === Scissors = Win    -- Paper/Scissors
Scissors === Scissors = Tie -- Scissors/Scissors

(/===) :: Shape -> Shape -> Verdict -- rig the results posthumously
Rock /=== Rock = Loss         -- Rock/"Loss"
Paper /=== Rock = Loss       -- Paper/"Loss"
Scissors /=== Rock = Loss     -- Scissors/"Loss"
Rock /=== Paper = Tie        -- Rock/Paper etc
Paper /=== Paper = Tie       -- Paper/Paper
Scissors /=== Paper = Tie   -- Scissors/Paper
Rock /=== Scissors = Win    -- Rock/Scissors
Paper /=== Scissors = Win    -- Paper/Scissors
Scissors /=== Scissors = Win -- Scissors/Scissors

-- A and X are Rock
-- B and Y are Paper
-- C and Z are Scissors

getShapeFromLetter :: String -> Shape -- extract shape from string
getShapeFromLetter x | x == "A" || x == "X" = Rock
                     | x == "B" || x == "Y" = Paper
                     | x == "C" || x == "Z" = Scissors
                     | otherwise = undefined

winningShape :: Shape -> Shape
winningShape s | s == Rock = Paper
               | s == Paper = Scissors
               | s == Scissors = Rock
               | otherwise = undefined

losingShape :: Shape -> Shape
losingShape s | s == Rock = Scissors
              | s == Paper = Rock
              | s == Scissors = Paper
              | otherwise = undefined

getVerdict :: [Shape] -> Verdict
getVerdict x = head x === last x

getRiggedVerdict :: [Shape] -> Verdict
getRiggedVerdict x = head x /=== last x

modifyShape :: [Shape] -> [Shape]
modifyShape x | getRiggedVerdict x == Tie = [head x, head x]
              | getRiggedVerdict x == Win = [head x, winningShape $ head x]
              | getRiggedVerdict x == Loss = [head x, losingShape $ head x]
              | otherwise = undefined

pointsOfVerdict :: Verdict -> Int
pointsOfVerdict Win = 6
pointsOfVerdict Loss = 0
pointsOfVerdict Tie = 3
pointsOfVerdict Mmm = -1

pointsOfShape :: Shape -> Int
pointsOfShape Rock = 1
pointsOfShape Paper = 2
pointsOfShape Scissors = 3

getPoints :: [Shape] -> Int
getPoints x = pointsOfShape (last x) + pointsOfVerdict (getVerdict x)

getPointsPart2 :: [Shape] -> Int
getPointsPart2 y = pointsOfShape (last x) + pointsOfVerdict (getVerdict x)
    where x = modifyShape y

main :: IO ()
main = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let l = lines contents
    print $ sum $ map (getPoints . (map getShapeFromLetter . words)) l
    print $ sum $ map (getPointsPart2 . (map getShapeFromLetter . words)) l
    hClose handle