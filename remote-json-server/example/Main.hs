module Main where
        
import Untyped
import Session       

main = do
        putStrLn "## Untyped, with wreq and scotty ##"
        sequence_ [ do s <- sIO
                       untyped s
                  | sIO <- sessions 
                  ]
