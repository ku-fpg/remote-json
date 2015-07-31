module Main where
        
import Untyped
import Session       

main = do
        putStrLn "## Untyped ##"
        sequence_ [ untyped s
                  | s <- sessions 
                  ]
