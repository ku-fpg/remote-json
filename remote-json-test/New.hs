{-# LANGUAGE OverloadedStrings #-}

module New 
 where

import Control.Remote.Monad.Fresh
import Control.Remote.Monad.JSON.Debug
import Control.Remote.Monad.JSON (Args(..), SendAPI(..))


import  qualified         Data.Text as Text
import           Data.Aeson 
import           Control.Remote.Monad.JSON.Client



testme :: IO ()
testme = do
  let s = weakSession (traceSendAPI "Justin" (clientSendAPI "http://www.raboof.com/projects/jayrock/demo.ashx"))
  v <- send s $ do
          method "echo" (List [String "Weak Method"])
  v <- send s $ do
     method "echo" (List [String "Hello, World"]) 
  print v   
  v <- send s $ do
     method "add" (List [Number 1, Number 2])
  print v   
  
  let ss = strongSession (clientSendAPI "http://www.raboof.com/projects/jayrock/demo.ashx")
  v <- send ss $ do
      v1 <- method "echo" (List [String "Strong Method"])
      return v1
  print v

  send s $ do
     notification "echo" (List [String "Weak Notification"])

