{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Debug.Trace

anyMany cs ps  =  foldr (\p rs -> rs ++ [(\cs -> parse p cs) ]) [] ps


data JSONValue = B Bool | S String | A [JSONValue] | O [(String, JSONValue)] deriving (Show)

ws :: Parser String
ws = many (oneOf " \t\n") 

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

-- ?????
comment :: Parser String
comment = string "//"
    *>
    (many (noneOf ['\n']))


lexeme :: Parser a -> Parser a
lexeme p = p <* ws

matchTrue :: Parser String 
matchTrue = string "true"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

matchFalse :: Parser String 
matchFalse = string "false"

alwaysFalse :: Parser Bool
alwaysFalse = pure False


boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue
boolFalse :: Parser Bool
boolFalse = matchFalse *> alwaysFalse

bool :: Parser Bool
bool = boolTrue <|> boolFalse

-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <$> :: (a -> b) -> Parser a -> Parser b

--  
jsonBool' :: Parser JSONValue
-- i.e 
-- jsonBool = fmap B bool
jsonBool' = B <$> bool

jsonBool = lexeme jsonBool'


jsonStringLiteral :: Parser JSONValue
jsonStringLiteral = lexeme (S <$> stringLiteral)
comma :: Parser Char
comma = char ','
array :: Parser [JSONValue]
array = 
    (lexeme $ char '[' )
    *>
    (jsonValue `sepBy` (lexeme comma ))
    <*
    (lexeme $ char ']')


jsonArray :: Parser JSONValue
-- fmap the constructor A over array to give JSON value
jsonArray = A <$> array     

objectEntry :: Parser (String, JSONValue)
objectEntry = do
    key <- stringLiteral
    char ':'
    val <- jsonValue
    return (key, val)

jsonObject :: Parser JSONValue
jsonObject = 
    O <$> (char '{'
          *>
          (objectEntry `sepBy` comma )
          <*
          char '}')
-- <?> :: Parse a -> String -> Parser a
jsonValue :: Parser JSONValue
jsonValue = 
         (jsonBool          <?> "JSON Bool")
     <|> (jsonStringLiteral <?> "JSON String literal")
     <|> (jsonArray         <?> "JSON Array")
     <|> (jsonObject        <?> "JSON Object")

 









-- parse matchTrue "etetet" "true"

tp :: (Parser a) -> String -> Either ParseError a
tp p txt = (parse p "" txt )

stringLiteral :: Parser String
stringLiteral = 
    char '"'
    *> 
    ( many (noneOf ['"'] ) )
    <* 
    char '"'

