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
 mainG (fromJust idx) (State [])
 return ()
mainF _ = undefined

mainG :: Int -> State -> IO State
mainG idx state = do
 change <- mainH idx
 if (getChangeCcfg change == Type.Emergs) then
  (return state)
 else
  (mainI state change >>= mainG idx)

mainHX :: Change -> IO ()
mainHX (Change (ChangeA1 a1 a2 a3 a4) a5) = do
    (if (a1 == Numerics) then putStrLn "mainHx ok" else putStrLn "mainHx oops")
    (if (a5 == ChangeA5Bs) then putStrLn "mainHx oops" else putStrLn "mainHx ok")

mainH :: Int -> IO Change
mainH idx = do
 change <- readChange idx
 mainHX change
 return change

mainI :: State -> Change -> IO State
mainI state change = do
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 return state
