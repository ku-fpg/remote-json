{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server (session) where

import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

session :: IO(Session) --   sync                                async
session = defaultSession Weak (fmap (fromMaybe Null) . router db) (void . router db)

db :: [(Text, [Value] -> IO Value)]
db = [ ("say", say) 
     , ("temperature", temperature)
     , ("fib", fib)
     ]
  where
        say :: [Value] -> IO Value
        say [ String txt ] = do 
                IO.putStrLn $ "remote: " <> txt
                return Null
        temperature :: [Value] -> IO Value
        temperature _ = do 
                t <- randomRIO (50, 100 :: Int)
                IO.putStr $ "temperature: "
                return $ toJSON t

        fib :: [Value] -> IO Value
        fib [Number x] = return $ toJSON (fibhelper x :: Int)

        fibhelper x
                  |x <= 1 = 1
                  |otherwise = fibhelper (x-2) + fibhelper (x-1)
