{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe

import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)

import           Control.Monad.Remote.JSON.Router
import           Control.Monad.Remote.JSON.Types -- for now
import           Data.Attoparsec.ByteString
import           Test (parseTest)

f1 :: Call a -> IO a
f1 (Method "subtract" [Number a,Number b] _) = return $ Number (a - b)
f1 _ = fail "f1"

-- Avoid skolem
newtype C = C (forall a . Call a -> IO a)

main = sequence_ 
        [ do let v_req = fromJust $ decode e
             putStrLn $ ("--> " ++) $ LT.unpack $ decodeUtf8 $ encode v_req
             r <- router sequence f (Send v_req)
             case r of
               Nothing -> putStrLn "<-- // Nothing"
               Just v_rep -> putStrLn $ ("<-- " ++) $ LT.unpack $ decodeUtf8 $ encode v_rep
             putStrLn ""
        | (C f,e) <- [ (C f1, "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}")
                     ]
        ]

