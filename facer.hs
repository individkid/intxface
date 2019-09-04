--
--    facer.hs
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types
import Foreign.C.String
import System.Environment
import System.Exit

foreign import ccall "forkExec" forkExecC :: CString -> IO ()
foreign import ccall "pipeInit" pipeInitC :: CString -> CString -> IO ()
foreign import ccall "waitAny" waitAnyC :: IO CInt
foreign import ccall "checkRead" checkReadC :: CInt -> IO CInt
foreign import ccall "checkWrite" checkWriteC :: CInt -> IO CInt
foreign import ccall "sleepSec" sleepSecC :: CInt -> IO ()
foreign import ccall "readStr" readStrC :: CInt -> IO CString
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "writeStr" writeStrC :: CString -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: CDouble -> CInt -> IO ()

forkExec :: String -> IO ()
forkExec a = (newCString a) >>= forkExecC
pipeInit :: String -> String -> IO ()
pipeInit a b = (newCString a) >>= (\x -> (newCString b) >>= (pipeInitC x))
waitAny :: IO Int
waitAny = fmap fromIntegral waitAnyC
checkRead :: Int -> IO Int
checkRead a = fmap fromIntegral (checkReadC (fromIntegral a))
checkWrite :: Int -> IO Int
checkWrite a = fmap fromIntegral (checkWriteC (fromIntegral a))
sleepSec :: Int -> IO ()
sleepSec a = sleepSecC (fromIntegral a)
readStr :: Int -> IO String
readStr a = (readStrC (fromIntegral a)) >>= peekCString
readInt :: Int -> IO Int
readInt a = fmap fromIntegral (readIntC (fromIntegral a))
readNum :: Int -> IO Double
readNum a = (readNumC (fromIntegral a)) >>= (\(CDouble x) -> return x)
writeStr :: String -> Int -> IO ()
writeStr a b = (newCString a) >>= (\x -> writeStrC x (fromIntegral b))
writeInt :: Int -> Int -> IO ()
writeInt a b = writeIntC (fromIntegral a) (fromIntegral b)
writeNum :: Double -> Int -> IO ()
writeNum a b = writeNumC (CDouble a) (fromIntegral b)

data MainABC = MainA Int | MainB Double | MainC String deriving (Show)
mainA :: [String]
mainA = ["a.out","b.out","facer.lua"]
mainB :: [[MainABC]]
mainB = [[MainA 0, MainB 0.1, MainC "zero"],[MainA 1, MainB 1.1, MainC "one"],[MainA 2, MainB 2.1, MainC "two"]]
mainD :: Int
mainD = 3

readMain :: MainABC -> Int -> IO MainABC
readMain (MainA _) a = fmap MainA (readInt a)
readMain (MainB _) a = fmap MainB (readNum a)
readMain (MainC _) a = fmap MainC (readStr a)

writeMain :: MainABC -> Int -> IO ()
writeMain (MainA a) b = writeInt a b
writeMain (MainB a) b = writeNum a b
writeMain (MainC a) b = writeStr a b

compMain :: MainABC -> MainABC -> Bool
compMain (MainA a) (MainA b) = a == b
compMain (MainB a) (MainB b) = a == b
compMain (MainC a) (MainC b) = a == b
compMain _ _ = False

main :: IO ()
main = getArgs >>= mainF -- mainF

mainF :: [String] -> IO ()
mainF [] = do
 mainFF mainA -- start processes
 sleepSec 1
 mainFG 0 mainB -- send stimulus
 actual <- mainFH mainB mainB -- collect responses
 mainFI actual mainB -- check responses
 mainFJ 0 -- wait processes
 check <- mainFK 0 [] -- check processes
 print ("facer.hs " ++ (show check))
mainF [a,b,c] = do
 pipeInit a b
 mainFL (head mainB) c -- copy request to response in order given
mainF _ = undefined

mainFF :: [String] -> IO ()
mainFF [] = return ()
mainFF (a:b) = (forkExec a) >> (mainFF b)

mainFG :: Int -> [[MainABC]] -> IO ()
mainFG _ [] = return ()
mainFG a (b:c) = (mainFGF a b) >> (mainFG (a+1) c)

mainFGF :: Int -> [MainABC] -> IO ()
mainFGF _ [] = return ()
mainFGF a (b:c) = (writeMain b a) >> (mainFGF a c)

mainFH :: [[MainABC]] -> [[MainABC]] -> IO [[MainABC]]
mainFH a b
 | (sum (map length a)) == 0 = return b
 | otherwise = do
  index <- waitAny
  mainFHF a b index

mainFHF :: [[MainABC]] -> [[MainABC]] -> Int -> IO [[MainABC]]
mainFHF a b c
 | (length (a !! c)) == 0 = do
  readInt c
  writeInt (negate 1) c
  mainFH a b
 | otherwise = do
  pattern <- mainFHJ b c
  actual <- readMain pattern c
  expect <- mainFHJ b c
  delete <- mainFHH a c
  remove <- mainFHH b c
  mainFHG delete remove c actual expect

mainFHG :: [[MainABC]] -> [[MainABC]] -> Int -> MainABC -> MainABC -> IO [[MainABC]]
mainFHG a b c d e
 | compMain d e = do
  insert <- mainFHI b c d
  mainFH a insert
 | otherwise = do
  print ("mismatch " ++ (show d) ++ " " ++ (show c))
  exitWith (ExitFailure (negate 1))

--mainFHH pop value at given index
mainFHH :: [[MainABC]] -> Int -> IO [[MainABC]]
mainFHH a b = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in return (pre ++ [(tail mid)] ++ post)

--mainFHI push value at given index
mainFHI :: [[MainABC]] -> Int -> MainABC -> IO [[MainABC]]
mainFHI a b c = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in return (pre ++ [(mid ++ [c])] ++ post)

--mainFHJ peek value at given index
mainFHJ :: [[MainABC]] -> Int -> IO MainABC
mainFHJ a b = let
 rest = drop b a
 mid = head rest
 in return (head mid)

mainFI :: [[MainABC]] -> [[MainABC]] -> IO ()
mainFI [] [] = return ()
mainFI (a:b) (c:d)
 | mainFIF a c = mainFI b d
 | otherwise = undefined
mainFI _ _ = undefined

mainFIF :: [MainABC] -> [MainABC] -> Bool
mainFIF [] [] = True
mainFIF (a:b) (c:d)
 | compMain a c = mainFIF b d
 | otherwise = False
mainFIF _ _ = False

mainFJ :: Int -> IO ()
mainFJ a
 | a == mainD = return ()
 | otherwise = (readInt a) >> (writeInt 0 a) >> (mainFJ (a+1))

mainFK :: Int -> [Int] -> IO [Int]
mainFK a b
 | a == mainD = return b
 | otherwise = do
  read <- checkRead a
  write <- checkWrite a
  mainFK (a+1) (b ++ [read,write])

mainFL :: [MainABC] -> String -> IO ()
mainFL [] _ = return ()
mainFL (a:b) c = do
 index <- waitAny
 value <- readMain a index
 writeMain value index
 mainFL b c
