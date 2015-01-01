{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hi.Config
    (
      parseConfig
    ) where

import           Control.Applicative   ((<$>), (<*))
import           Data.Maybe            (catMaybes)
import           Text.Parsec
import           Text.Parsec.String

-- | Parse config file
parseConfig :: String -> [(String, String)]
parseConfig x = case parse configFile "Invalid config file format" x of
      Left  l  -> error $ show l
      Right xs -> xs

configFile :: Parser [(String, String)]
configFile = catMaybes <$> many line <* eof

sep :: Parser Char
sep = char ':'

name :: Parser String
name = many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['-'])

eol :: Parser Char
eol = newline <|> (eof >> return '\n')

comment :: Parser ()
comment = do
    _ <- char '#' <* manyTill anyChar newline
    return ()

line :: Parser (Maybe (String, String))
line = do
    spaces
    try (comment >> return Nothing) <|> (line' >>= return . Just)
  where
    line' = do
        spaces
        n <- name
        spaces >> sep >> spaces
        v <- many (noneOf "\n")
        eol
        return (n, v)
