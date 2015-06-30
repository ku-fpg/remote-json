{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat,(<>))
import Data.Aeson
import Data.Text
import System.Random
import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as IO
import Control.Monad.Remote.JSON (router)
import Data.Maybe




main = scotty 3000 $ do
      post "/" $do
            d <- jsonData
            v<- liftIO $ router db d
            Web.Scotty.json $ fromMaybe Null v



db :: [(Text, [Value] ->IO Value)]
db = [("say",say)
     ,("temperature",temperature)
     ,("fib", fib)
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
