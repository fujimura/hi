{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Distribution.Hi.Config
    (
      parseConfig
    ) where

import           Control.Applicative   ((<$>), (<*))
import           Data.Maybe            (catMaybes)
import           Distribution.Hi.Types
import           Text.Parsec
import           Text.Parsec.String

sep :: Parser Char
sep = char ':'

name :: Parser String
name = many (oneOf $ ['a'..'z'] ++ ['A'..'Z'])

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

configFile :: Parser [(String, String)]
configFile = catMaybes <$> many line <* eof

parseConfig :: String -> IO [Arg]
parseConfig x = case parse configFile "ERROR" x of -- TODO Error message
      Left  l  -> error $ show l
      Right xs -> return $ map (uncurry Val) xs
