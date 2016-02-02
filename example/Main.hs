module Main where
        
import Untyped
import Typed
import DSL
import Session       

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
