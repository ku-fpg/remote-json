{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Untyped where

import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

import Session

untyped :: Session -> IO ()
untyped s = do
        t <- send s $ do
                notification "say" (List [String "Hello, "])
                notification "say" (List [String "World!"])
                t <- method "temperature" None
                return t
        print t                      
        r <- send s $ method "fib" (List [Number 10])
        print r
