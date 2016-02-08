{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Untyped where

import Control.Remote.Monad.JSON
import Data.Aeson

untyped :: Session -> IO ()
untyped s = do
        t <- send s $ do
                notification "say" (List [String "Hello, "])
                notification "say" (List [String "World!"])
                t <- method "temperature" None
                return t
        print t                    
        r <- send s $ method "fib" (List [Number 10])
        print r
