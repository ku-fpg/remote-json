{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad.Remote.JSON as Remote
import Control.Monad.Remote.JSON
import Data.Aeson
import Control.Monad (void)
import qualified VChanUtil as VChan
import VChanUtil 

createSession :: LibXenVChan -> Session
createSession chan = defaultSession Strong (\v-> do 
                                                 putStrLn $ "Sync: " ++ (show v)
                                                 VChan.send chan (encode v)
                                                 val <- receive chan
                                                 let res = decode val :: Maybe Value
                                                 putStrLn $ "Sync Got back: "++(show res)
                                                 case res of 
                                                   Just x -> return x
                                                   Nothing -> return Null
                                           )
                                           (\v ->do
                                                 putStrLn $ "Async: " ++ (show v)
                                                 VChan.send chan (encode v)
                                                 _ <- receive chan ::IO String
                                                 return () 
                                           )


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
          chan <- client_init other
          let session =  createSession chan
          t<- Remote.send session $ do
             notification "say" [String "Hello"] 
             notification "say" [String "Howdy"] 
             notification "say" [String "GoodDay"] 
             method "temperature" []
          putStr "Temperature: "
          print t 
          
