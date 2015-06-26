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
        t <- send session $ do
                notification "say" [String "Hello!"]
                notification "say" [String "Hola!"]
                t <- method "temperature" [] 
                notification "say" [String "Yoohoo!"]
                notification "say" [String "Aloha!"]
                return t
        print t                      
        
