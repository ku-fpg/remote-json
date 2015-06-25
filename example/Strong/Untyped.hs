{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

import Server

main = do
        s <- session
        t <- send s $ do
                notification "say" [String "Hello!"]
                notification "say" [String "Hello!"]
                t <- method "temperature" [] 
                return t
        print t                      
        
