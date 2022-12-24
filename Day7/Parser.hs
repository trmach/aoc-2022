{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser where

import Data.Char

newtype State s a = State { run :: s -> Maybe (a, s) } -- run: accept state and possibly return a new state and a value

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f st = State $ \s -> case run st s of Nothing -> Nothing
                                               Just (x, s') -> Just (f x, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> Just (x, s)
    
    (<*>) :: State s (a -> b) -> State s a -> State s b
    stf <*> stx = State $ \s -> case run stf s of Nothing -> Nothing
                                                  Just (f, s') -> run (f <$> stx) s'

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    st >>= f = State $ \s -> case run st s of Nothing -> Nothing
                                              Just (x, s') -> run (f x) s'

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    many :: f a -> f [a]
    many x = some x <|> pure []

    some :: f a -> f [a]
    some x = ((:) <$> x) <*> many x

instance Alternative (State s) where
    empty :: State s a
    empty = State $ const Nothing

    (<|>) :: State s a -> State s a -> State s a
    p <|> q = State $ \s -> case run p s of Nothing -> run q s
                                            r -> r

type Parser a = State String a

item :: Parser Char
item = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)
                              
sat :: (Char -> Bool) -> Parser Char    
sat p = do
    x <- item
    if p x then return x else State (const Nothing)

unsat :: (Char -> Bool) -> Parser Char
unsat p = do
    x <- item
    if p x then State (const Nothing) else return x

char :: Char -> Parser Char
char c = sat (==c)

unchar :: Char -> Parser Char
unchar c = unsat (==c)

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
    char x
    string xs
    return $ x:xs

space :: Parser ()
space = do
    many $ sat isSpace
    return ()

token :: Parser a -> Parser a
token p = do
    space
    x <- p
    space
    return x

symbol :: String -> Parser String
symbol s = token (string s)

digit :: Parser Char
digit = sat isDigit

digits :: Parser [Char]
digits = some digit

nat :: Parser Int
nat = read <$> digits

int :: Parser Int
int = (do
    char '-'
    n <- nat
    return (-n)) <|> nat

--line :: Parser String
--line = do some (unsat (=='\n'))

prompt :: Parser String
prompt = symbol "$"

parseCd :: Parser String
parseCd = symbol "cd"
          
parseLs :: Parser String -- simply return "ls"
parseLs = symbol "ls"

identifier :: Parser String -- we will use this to label files and dirs
identifier = token (some (sat isAlphaNum <|> sat (=='.') <|> sat (=='/')))
-- when identifier is ".." go back, if identifier has '.' it's a file, etc
-- we will use "nat" to get the size

getDir :: Parser String
getDir = do symbol "dir"
            identifier -- return the name

getFile :: Parser Int -- only consider the size of files, they do not need to be uniquely identifiable
getFile = do size <- nat
             identifier
             return size

parseInstruction :: Parser String
parseInstruction = do prompt
                      cmd <- parseCd <|> parseLs
                      case cmd of "ls" -> return "Contents: " {- parseDirContents -}
                                  "cd" -> (do i <- identifier
                                              case i of ".." -> return "Going back one dir."
                                                        _ -> return ("Going to " ++ i)) {- get dir indentifier -}
                                  _ -> return "bitchass" -- definitely undefined so throw error

parseDirContents :: Parser [String] -- Int will be the sum of the immediate contents
parseDirContents = some (do t <- unsat (=='$')
                            item <- identifier -- sometimes item is "dir", sometimes it is a number
                            name <- identifier -- this is the name of the dir or file
                            if isDigit t 
                              then return ("File: " ++ name ++ "\nSize: " ++ t:item)
                              else return ("Dir: " ++ name))
