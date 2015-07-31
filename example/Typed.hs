{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typed (typed) where

import Control.Monad        
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

import DSL 

typed :: DSLSession -> IO ()
typed s = do
        ts <- send s $ do
                say "Hello, "
                say "World!"
                t1 <- temperature
                t2 <- temperature
                t3 <- temperature
                say "Howdy"
                return [t1,t2,t3]
        print ts                       
        r <- send s (fib 10)
        print r

