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
                method "say" [String "Hello!"]
                method "say" [String "Hello!"]
                t :: Result Int <- fromJSON <$> method "temperature" []
                return t
        print t                        
        