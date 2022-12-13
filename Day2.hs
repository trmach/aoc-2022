import System.IO
import Control.Monad

filename = "day-2.txt"

main :: IO ()
main = do
    let list = []
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    print $ lines contents
    hClose handle