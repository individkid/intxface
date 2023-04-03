module Main where

import Naive
import Face
import Type
import System.Environment

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 putStrLn (show (boolToSide False))
 idx <- wrapInit a
 sculpt <- readSculpt idx
 mainG sculpt
mainF _ = undefined

mainG :: Sculpt -> IO ()
mainG _ = undefined

