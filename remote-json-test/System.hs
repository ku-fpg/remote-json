module Main where
        
import Untyped
import Typed
import DSL
import Session       

import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Router (transport)
import Control.Remote.Monad.JSON.Server
import Control.Remote.Monad.JSON.Client
import Control.Natural
import Control.Concurrent (forkIO,threadDelay)

{-
  [ do let port = 4000 + n + m
       print port
       forkIO $ serverReceiveAPI port "/test-remote-json" r
       threadDelay (1000 * 1000)
       return $ f $ clientSendAPI ("http://localhost:" ++ show port ++ "/test-remote-json")
-}


sessions :: IO [Session]
sessions = do
  rbs <- sequence
        [ do forkIO $ serverReceiveAPI port "/test-remote-json" rb 
             threadDelay (1000 * 1000)
             return $ clientSendAPI ("http://localhost:" ++ show port ++ "/test-remote-json")
        | (rb,port) <- routerBuilders `zip` [4000..]
        ]
  return 
    [ sb rb
    | sb <- sessionBuilders
    , rb <- rbs
    ]

main:: IO()
main = do
        ss <- sessions
        putStrLn "## Untyped ##"
        sequence_ [ untyped s
                  | s <- ss
                  ]
        putStrLn "## Typed ##"
        sequence_ [ typed (DSLSession s)
                  | s <- ss 
                  ]
