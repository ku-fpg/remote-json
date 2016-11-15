{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Natural
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe

import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)

import           Control.Monad (when)
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON
import           Data.Attoparsec.ByteString
import           System.Exit
import           Test (readTests, Test(..))

f :: Call a -> IO a
f (CallMethod "subtract" (List [Number a,Number b])) = return $ Number (a - b)
f (CallMethod "subtract" (Named xs))
        | Just (Number a) <- lookup "minuend" xs
        , Just (Number b) <- lookup "subtrahend" xs
        = return $ Number (a - b)
f (CallMethod "sum" args) = case args of
      List xs -> return $ Number $ sum $ [ x | Number x <- xs ]
      _ -> invalidParams
f (CallMethod "get_data" None) = return $ toJSON [String "hello", Number 5]
f (CallMethod "error" (List [String msg])) = error $ show msg
f (CallMethod "fail" (List [String msg])) = fail $ show msg
f (CallNotification "update" _) = return $ ()
f (CallNotification "notify_hello" _) = return $ ()
f (CallNotification "notify_sum" _)   = return $ ()
f _ = methodNotFound

main = do
  tests <- readTests "tests/spec/Spec.txt"
  let testWith i testName (Right v_req) v_expect = do
             putStrLn $ ("--> " ++) $ LT.unpack $ decodeUtf8 $ encode v_req
             r <- router sequence (wrapNT f) # (Receive v_req)
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
