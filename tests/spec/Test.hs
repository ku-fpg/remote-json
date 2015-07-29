{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test where

import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Maybe
import           Control.Applicative

import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Encoding(decodeUtf8)
import qualified Data.ByteString as BS

import           Control.Monad
import           Control.Monad.Remote.JSON.Router
import           Control.Monad.Remote.JSON.Types -- for now
import           Data.Attoparsec.ByteString hiding (parseTest)


data Test = Test Text [(Value,Maybe Value)]
  deriving Show

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

ws :: Parser ()
ws = many (skip (inClass " \n")) *> return ()

parseTest :: Parser Test
parseTest = do
        title <- lexeme (string "#" *> some (satisfy (notInClass "\n")))
        ts <- many $ do
                lexeme (string "-->") 
                req <- json'
                res <- optional (lexeme (string "<--") *> json')
                return $ (req,res)
        return $ Test (decodeUtf8 (BS.pack title)) ts

readTests :: FilePath -> IO [Test]
readTests fileName = do
        bs <- BS.readFile fileName
        case parseOnly (many parseTest <* ws) bs of
          Left msg -> error msg
          Right v  -> return v
