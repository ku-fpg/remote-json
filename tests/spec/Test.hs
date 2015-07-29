{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Maybe
import           Control.Applicative

import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)

import           Control.Monad
import           Control.Monad.Remote.JSON.Router
import           Control.Monad.Remote.JSON.Types -- for now
import           Data.Attoparsec.ByteString hiding (parseTest)


data Test = Test Value Value
  deriving Show

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

ws :: Parser ()
ws = many (skip (inClass " \n") <|> void (string "#")) *> return ()

parseTest :: Parser Test
parseTest = do
        lexeme (string "-->") 
        req <- json'
        lexeme (string "<--")
        res <- json'
        return $ Test req res      


