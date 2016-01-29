{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module New 
 where

import Control.Monad.Remote.Fresh
import Control.Monad.Remote.JSON.Debug
import           Control.Monad.Remote.JSON.Types (Args(..), SessionAPI(..))


import  qualified         Data.Text as Text
import           Data.Aeson 
import           Control.Monad.Remote.JSON.Client



testme :: IO ()
testme = do
  let s = weakSession (traceSessionAPI "Justin" (clientSessionAPI "http://www.raboof.com/projects/jayrock/demo.ashx"))
  v <- send s $ do
          method "echo" (List [String "Weak Method"])
  v <- send s $ do
     method "echo" (List [String "Hello, World"]) 
  print v   
  v <- send s $ do
     method "add" (List [Number 1, Number 2])
  print v   
  
  let ss = strongSession (clientSessionAPI "http://www.raboof.com/projects/jayrock/demo.ashx")
  v <- send ss $ do
      v1 <- method "echo" (List [String "Strong Method"])
      return v1
  print v

  send s $ do
     notification "echo" (List [String "Weak Notification"])

