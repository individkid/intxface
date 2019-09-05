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

data MainABC = MainA Int | MainB Double | MainC String deriving (Show,Eq)
mainA :: [String]
mainA = ["a.out","b.out","facer.lua"]
mainB :: [[MainABC]]
mainB = [[MainA 0, MainB 0.1, MainC "zero"],
         [MainA 1, MainB 1.1, MainC "one"],
         [MainA 2, MainB 2.1, MainC "two"]]
mainC :: [[MainABC]]
mainC = map mainCF mainB
mainD :: Int
mainD = length mainB

mainCF :: [MainABC] -> [MainABC]
mainCF = map mainCG

mainCG :: MainABC -> MainABC
mainCG (MainA a) = MainA (negate 1)
mainCG (MainB a) = MainB 0.0
mainCG (MainC a) = MainC ""

readMain :: MainABC -> Int -> IO MainABC
readMain (MainA _) a = fmap MainA (readInt a)
readMain (MainB _) a = fmap MainB (readNum a)
readMain (MainC _) a = fmap MainC (readStr a)

writeMain :: MainABC -> Int -> IO ()
writeMain (MainA a) b = writeInt a b
writeMain (MainB a) b = writeNum a b
writeMain (MainC a) b = writeStr a b

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [] = do
 mainFF mainA -- start processes
 sleepSec 1
 mainFG 0 mainB -- send stimulus
 mainFH mainC mainB -- check responses
 check <- mainFI 0 [] -- check processes
 print ("facer.hs " ++ (show check))
mainF [a,b,c] = do
 pipeInit a b
 mainFJ (mainC !! (read c)) -- copy request to response in order given
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

mainFH :: [[MainABC]] -> [[MainABC]] -> IO ()
mainFH a b = waitAny >>= (mainFHF a b)

mainFHF :: [[MainABC]] -> [[MainABC]] -> Int -> IO ()
mainFHF a b c
 | c == mainD = return ()
 | (length (a !! c)) == 0 =
  (readInt c) >> (writeInt (negate 1) c) >> (mainFH a b)
 | otherwise =
  (readMain d c) >>= (mainFHG (mainFHH a c) (mainFHH b c) c d)
 where d = mainFHJ b c

mainFHG :: [[MainABC]] -> [[MainABC]] -> Int -> MainABC -> MainABC -> IO ()
mainFHG a b c d e
 | d == e = mainFH a (mainFHI b c d)
 | otherwise = (print ("mismatch " ++ (show e) ++ " " ++ (show c))) >>
  (exitWith (ExitFailure (negate 1)))

--mainFHH pop value at given index
mainFHH :: [[MainABC]] -> Int -> [[MainABC]]
mainFHH a b = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in pre ++ [(tail mid)] ++ post

--mainFHI push value at given index
mainFHI :: [[MainABC]] -> Int -> MainABC -> [[MainABC]]
mainFHI a b c = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in pre ++ [(mid ++ [c])] ++ post

--mainFHJ peek value at given index
mainFHJ :: [[MainABC]] -> Int -> MainABC
mainFHJ a b = let
 rest = drop b a
 mid = head rest
 in head mid

mainFI :: Int -> [Int] -> IO [Int]
mainFI a b
 | a == mainD = return b
 | otherwise = do
  read <- checkRead a
  write <- checkWrite a
  mainFI (a+1) (b ++ [read,write])
 
mainFJ :: [MainABC] -> IO ()
mainFJ [] = return ()
mainFJ (a:b) = do
 index <- waitAny
 value <- readMain a index
 writeMain value index
 mainFJ b
