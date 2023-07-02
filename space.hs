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
 idx <- wrapIdent Type.Spacez a
 sculpt <- mainG idx
 mainH sculpt
mainF _ = undefined

mainG :: Maybe Int -> IO (Maybe Sculp)
mainG Nothing = return Nothing
mainG (Just idx) = readSculp idx >>= return . Just

mainH :: Maybe Sculp -> IO ()
mainH = undefined

