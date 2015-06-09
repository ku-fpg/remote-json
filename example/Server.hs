{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server (session) where

import Control.Monad        
import Control.Monad.Remote.JSON_Strong
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

session = defaultSession 
 { sync  = fmap (fromMaybe Null) . router db
 , async = void                  . router db
 }

db :: [(Text, [Value] -> IO Value)]
db = [ ("say", say) 
     , ("temperature", temperature)
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
