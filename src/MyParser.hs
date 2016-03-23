{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
module MyParser where

import Debug.Trace
import Control.Applicative 
newtype Parser a = Parser (String -> [(a, String)]) 




item :: Parser Char
item = Parser (\cs -> case cs of
                           ""     -> []
                           (c:cs) -> [(c, cs)])

-- eg
-- newtype Firstname = Firstname String
-- unFirstname :: Firstname -> String
-- unFirstname (Firstname n) = n 

parse :: Parser t -> String -> [(t, String)]
parse (Parser p) = p

class Monad m => MonadZero m where
    zero :: m a
   -- (+=+) :: m a -> m a -> m a

class MonadZero m => MonadPlus m where
    (+=+) :: m a -> m a -> m a

instance MonadZero Parser where 
    zero = Parser (\cs -> [])

instance MonadPlus Parser where
    p +=+ q = Parser (\cs -> parse p cs +=+ parse q cs)

instance MonadPlus [] where
    l1 +=+ l2 = l1 ++ l2

instance MonadZero [] where
    zero = []

instance Monad Parser  where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p +=+ q) cs of
                           []     -> []
                           (x:_) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else zero

   

-- anyMany :: String -> [Parser a] -> [Parser a]
anyMany cs ps  = Parser <$>  foldr (\p rs -> rs ++ [(\cs -> parse p cs) ]) [] ps

test = anyMany "abd" [item,item] 
