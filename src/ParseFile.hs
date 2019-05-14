{-# LANGUAGE OverloadedStrings #-}

module ParseFile(parseFile) where

import qualified Control.Applicative as CA
import Data.Attoparsec.Text as A
import Data.List
import qualified Data.Text as T

-- | Parse heads of columns.

parseHead :: Parser [T.Text]
parseHead = manyTill (T.pack <$> (many1 (notChar ' ') <* CA.many (char ' '))) (string "\n" CA.<|> string "\r\n")

-- | Parse columns of data.

parseData :: Parser [[Float]]
parseData = transpose <$> sepBy (sepBy (realToFrac <$> A.double) (CA.many $ char ' ')) (CA.many (char ' ') <* (string "\n" CA.<|> string "\r\n"))

-- | Parse file with data

parseFile :: Parser [(T.Text,[Float])]
parseFile = zip <$> parseHead <*> parseData

