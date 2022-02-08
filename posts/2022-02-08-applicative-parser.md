---
title: Applicative Parser
---

This article is only for fun XD

An applicative parser is less powerful than a monadic parser, but
it can almost do what monadic parsers can do.

Before starting, let's import some useful stuffs and extensions.

``` haskell
{-# LANGUAGE MonadComprehensions #-}
import           Control.Applicative            ( Alternative((<|>), empty) )
```

Let's first define a traditional parser type:

``` haskell
newtype Parser a = Parser {parse :: String -> Maybe (a, String)}
```

As usual, define `Functor` and `Applicative`:

``` haskell
instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [ (f a, s') | (a, s') <- p s ]

instance Applicative Parser where
    pure a = Parser $ \s -> pure (a, s)
    (Parser f) <*> (Parser p) =
        Parser $ \s -> [ (a b, s'') | (a, s') <- f s, (b, s'') <- p s' ]
```

Another useful typeclass is `Alternative`:

``` haskell
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s
```

Now, we can define our parser combinators :D

Let's start with some useful higher order ones:

``` haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \xs -> case xs of
    []       -> Nothing
    (s : ss) -> [ (s, ss) | p s ]

many :: Parser a -> Parser [a]
many p = Parser $ \s -> case parse p s of
    Nothing      -> Just ([], s)
    Just (a, s') -> case parse (many p) s' of
        Nothing        -> Just ([a], s')
        Just (a', s'') -> Just (a : a', s'')

-- this is just `asum`
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty
```

Now, the more useful ones:

``` haskell
char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

oneOf :: String -> Parser Char
oneOf = choice . map char

spaces :: Parser ()
spaces = many (oneOf " \n\t") *> pure ()

digits :: Parser String
digits = many1 $ oneOf ['0' .. '9']
```

And now we can do a small example to parse math expressions:

``` haskell
expr :: Parser Int
expr = flip ($) <$> factor <*> expr'

--  this is how we eliminate left recursion
expr' :: Parser (Int -> Int)
expr' =
    (\d f i -> f (i + d))
        <$> (char '+' *> factor)
        <*> expr'
        <|> (\d f i -> f (i - d))
        <$> (char '-' *> factor)
        <*> expr'
        <|> pure id

factor :: Parser Int
factor = flip ($) <$> term <*> factor'

factor' :: Parser (Int -> Int)
factor' =
    (\d f i -> f (i * d))
        <$> (char '*' *> term)
        <*> expr'
        <|> (\d f i -> f (i `div` d))
        <$> (char '/' *> term)
        <*> expr'
        <|> pure id

term :: Parser Int
term = (read <$> digits) <|> char '(' *> expr <* char ')'
```

Let give it a try!

``` haskell
> parse expr "(1+2)*(4/2)+6/(3-1)"
Just (9,"")
> parse expr "hello"
Nothing
```

And we're done XD
