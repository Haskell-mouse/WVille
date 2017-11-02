{-# LANGUAGE OverloadedStrings #-}

module ParseFile where 

import Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.List
import qualified Control.Applicative as CA 

-- | Parse heads of columns. 

parseHead :: Parser [T.Text]
parseHead = manyTill (T.pack <$> (many1 (notChar ' ') <* char ' ')) (string "\n" CA.<|> string "\r\n")

-- | Parse columns of data. 

parseData :: Parser [[Double]]
parseData = transpose <$> sepBy (sepBy A.double (CA.many $ char ' ')) (CA.many (char ' ') <* (string "\n" CA.<|> string "\r\n"))

-- | Parse file with data

parseFile :: Parser [(T.Text,[Double])]
parseFile = zip <$> parseHead <*> parseData

