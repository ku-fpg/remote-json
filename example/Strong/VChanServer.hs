{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Data.Monoid (mconcat,(<>))
import Data.Aeson
import Data.Text
import System.Random
--import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as IO
import Control.Monad.Remote.JSON (routerDebug)
import VChanUtil


prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to talk to?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop




main = do id <- getDomId
          putStrLn $ "ID: "++(show id)       
          other <- prompt     
          chan <- server_init other
          loop chan
             where loop chan =do 
                          res <-receive chan   
                          let d = decode res :: Maybe Value
                          case d of 
                             Just x -> do v <-routerDebug True db x
                                          putStrLn $ "Sending : "++(show v)
                                          send chan (encode v)
                             Nothing -> send chan (encode errorObj)
                          loop chan


errorObj :: Value
errorObj = object ["jsonrpc" .= ("2.0"::Text), "error".= object ["code" .= (-32600 :: Int), "message" .= ("Invalid Request"::Text)]]

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
