module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe

data State = State [Int]

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainI (fromJust idx) (State [])
 return ()
mainF _ = undefined

mainG :: Int -> IO Change
mainG idx = readChange idx >>= return

mainH :: State -> Change -> IO State
mainH state change = do
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 return state

mainI :: Int -> State -> IO State
mainI idx state = do
 change <- mainG idx
 -- if change is to terminate then (return state) else (mainH state change >>= mainI idx)
 mainH state change
