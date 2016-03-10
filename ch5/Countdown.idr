module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z             = putStrLn "Lift off!"
countdown (S remaining) = do putStrLn (show ( S remaining) )
                             usleep 1000000
                             countdown remaining
