{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Remote.JSON
import Network.Wreq
import Data.Aeson
import Control.Lens ((^.))
import Control.Monad (void)

session :: Session
session = defaultSession Strong (\v -> do
                                   r <- asJSON =<< post "http://129.237.120.52:3000/" v
                                   return $ r ^. responseBody) 
                                (\v -> do
                                  void $ post "http://129.237.120.52:3000/" v
                                  return ())


main = do t<- send session $ do
             notification "say" [String "Hello"] 
             notification "say" [String "Howdy"] 
             notification "say" [String "GoodDay"] 
             method "temperature" []
          print t 
