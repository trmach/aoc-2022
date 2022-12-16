{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}
import System.IO
import Control.Monad

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

instance Show Shape where
    show :: Shape -> String
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

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

-- A and X are Rock
-- B and Y are Paper
-- C and Z are Scissors

getShapeFromLetter :: String -> Shape -- extract shape from string
getShapeFromLetter x | x == "A" || x == "X" = Rock
                     | x == "B" || x == "Y" = Paper
                     | x == "C" || x == "Z" = Scissors
                     | otherwise = undefined

getVerdict :: [Shape] -> Verdict
getVerdict x = head x === last x

pointsOfVerdict :: Verdict -> Int
pointsOfVerdict Win = 6
pointsOfVerdict Loss = 0
pointsOfVerdict Tie = 3

pointsOfShape :: Shape -> Int
pointsOfShape Rock = 1
pointsOfShape Paper = 2
pointsOfShape Scissors = 3

getPoints :: [Shape] -> Int
getPoints x = pointsOfShape (last x) + pointsOfVerdict (getVerdict x)

main :: IO ()
main = do
    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print $ sum $ map (getPoints . (map getShapeFromLetter . words)) $ lines contents
    hClose handle