{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe

import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)

import           Control.Monad (when)
import           Control.Monad.Remote.JSON.Router
import           Control.Monad.Remote.JSON.Types -- for now
import           Data.Attoparsec.ByteString
import           System.Exit
import           Test (readTests, Test(..))

f :: Call a -> IO a
f (Method "subtract" (List [Number a,Number b]) _) = return $ Number (a - b)
f (Method "subtract" (Named xs) _)
        | Just (Number a) <- lookup "minuend" xs
        , Just (Number b) <- lookup "subtrahend" xs
        = return $ Number (a - b)
f (Method "sum" args _) = case args of
      List xs -> return $ Number $ sum $ [ x | Number x <- xs ]
      _ -> invalidParams
f (Method "get_data" None _) = return $ toJSON [String "hello", Number 5]
f (Notification "update" _) = return $ ()
f (Notification "notify_hello" _) = return $ ()
f (Notification "notify_sum" _)   = return $ ()
f (Method "error" (List [String msg]) _) = error $ show msg
f (Method "fail" (List [String msg]) _) = fail $ show msg
f _ = methodNotFound

-- Avoid skolem
newtype C = C (forall a . Call a -> IO a)

main = do
  tests <- readTests "tests/spec/Spec.txt"
  let testWith i testName (Right v_req) v_expect = do
             putStrLn $ ("--> " ++) $ LT.unpack $ decodeUtf8 $ encode v_req
             r <- router sequence f (Send v_req)
             showResult i testName r v_expect
      testWith i testName (Left bad) v_expect = do
             putStr "--> " 
             TIO.putStr $ bad
             let r = Just $ parseError
             showResult i testName r v_expect
      showResult i testName Nothing v_expect = do
             testResult i testName Nothing v_expect
      showResult i testName (Just v_resp) v_expect = do
             putStrLn $ ("<-- " ++) $ LT.unpack $ decodeUtf8 $ encode v_resp
             testResult i testName (Just v_resp) v_expect
             
      testResult i testName r v_expect = do
             r <- if (r /= v_expect) 
                  then do putStrLn $ ("exp " ++) $ LT.unpack $ decodeUtf8 $ encode v_expect
                          return $ Just (i,testName)
                  else return Nothing
             putStrLn ""
             return r

    
  res <- sequence 
        [ do when (i == 1) $ do
                putStr "#" 
                TIO.putStrLn $ testName
             testWith i testName v_req v_expect
        |  (Test testName subTests) <- tests
        ,  (i,(v_req,v_expect)) <- [1..] `zip` subTests
        ]
  let failing = [ x | Just x <- res ]
  if (null failing)
  then putStrLn $ "ALL " ++ show (length res) ++ " TEST(S) PASS"
  else do 
     putStrLn $ show (length failing) ++ " test(s) failed"
     putStrLn $ unlines $ map show failing
     exitFailure
