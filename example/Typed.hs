{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad        
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

import DSL 
import Server

main = do
        let s = Session session
        t <- send s $ do
                say "Hello!"
                say "Hello!"
                t <- temperature 7
                return t
        print t                        
        

