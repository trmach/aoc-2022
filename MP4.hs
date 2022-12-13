module MP4 where

import Data.Char


data State s a = State { run :: s -> Maybe (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')


instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


instance Alternative (State s) where
  empty = State $ \_ -> Nothing
  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r


type Parser a = State String a


item :: Parser Char
item = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else State (\_ -> Nothing)


notSat :: (Char -> Bool) -> Parser Char
notSat p = do x <- item
              if p x then State (\_ -> Nothing) else return x

char :: Char -> Parser Char
char c = sat (==c)

notChar :: Char -> Parser Char
notChar c = notSat (==c)


string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return $ x:xs


space :: Parser ()
space = do many $ sat isSpace
           return ()


token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x



symbol :: String -> Parser String
symbol s = token (string s)


digit :: Parser Char
digit = sat isDigit

digits''' :: Parser [Char]
digits''' = some digit

nat :: Parser Int
nat = read <$> digits'''

int :: Parser Int
int = (do char '-'
          n <- nat
          return (-n))
      <|> nat



{-
  Parses a limited C function in order to obtain:

  1. The name of the function
  2. A list of the names of the parameters of the function
  3. A list of the names of the local variables declared within the function
  4. The variable name or integer value returned by the function (as a string),
     or the empty string if there is no return statement.

  See the writeup for examples.
-}



identifier :: Parser String
identifier = token (some (sat isAlphaNum))

notSemicolon :: Parser String
notSemicolon = token (some (notChar ';'))
-- run (token (some (sat isAlphaNum))) $ "   hello1 world!" takes hello1 as function identifier

typeName :: Parser String
typeName = symbol "int" <|> symbol "char"
-- (symbol "char") $ "char foo1 (p1, p2,..) {} " will consume char, for example

funcDef :: Parser (String, [String], [String], String)
funcDef = do typeName
             id <- identifier -- id is bound to the value (String) just extracted
             symbol "("
             params <- paramList <|> return []
             symbol ")"
             symbol "{"
             decks <- many (do varDecls)
             asses <- many (do assignment)
             retVal <- returnStmt <|> return ""
             symbol "}"
             return (id,params,concat decks,retVal)
             
returnStmt :: Parser String
returnStmt = do symbol "return"
                retVal <- notSemicolon
                symbol ";"
                return retVal

paramList :: Parser [String]
paramList = do typeName
               param <- identifier
               params <- many (do symbol ","
                                  typeName
                                  identifier)
               return (param:params)
               

 



assignment :: Parser ()
assignment = do identifier
                symbol "="
                notSemicolon
                symbol ";"
                return ()

varDecls :: Parser [String]
varDecls = do typeName
              var <- identifier
              vars <- many (do symbol ","
                               identifier)
              symbol ";"
              return (var:vars)



