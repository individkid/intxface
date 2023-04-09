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

mainG :: Maybe Int -> IO (Maybe Sculpt)
mainG Nothing = return Nothing
mainG (Just idx) = readSculpt idx >>= return . Just

mainH :: Maybe Sculpt -> IO ()
mainH = undefined

