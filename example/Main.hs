module Main where
        
import Untyped
import Typed
import DSL
import Session       

import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Router (transport)
import Control.Natural


sessions :: [Session]
sessions = 
    [ sb (Nat $ transport (rb $$))
    | sb <- sessionBuilders
    , rb <- routerBuilders
    ]

main:: IO()
main = do
        putStrLn "## Untyped ##"
        sequence_ [ untyped s
                  | s <- sessions 
                  ]
        putStrLn "## Typed ##"
        sequence_ [ typed (DSLSession s)
                  | s <- sessions 
                  ]
