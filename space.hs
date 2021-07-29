module Main where

import Naive
import Face
import Type
import System.Environment

mainX :: IO ()
mainX = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a,b,c] = do
 putStrLn (show (boolToSide False))
 idx <- pipeInit a b
 sculpt <- readSculpt idx
 mainG sculpt
mainF _ = undefined

mainG :: Sculpt -> IO ()
mainG _ = undefined

