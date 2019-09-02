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

foreign import ccall "forkExec" forkExecC :: CString -> IO ()
foreign import ccall "pipeInit" pipeInitC :: CString -> CString -> IO ()
foreign import ccall "waitAny" waitAnyC :: IO CInt
foreign import ccall "checkRead" checkReadC :: CInt -> IO CInt
foreign import ccall "sleepSec" sleepSecC :: CInt -> IO ()
foreign import ccall "readString" readStringC :: CInt -> IO CString
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "writeString" writeStringC :: CString -> CInt -> IO ()
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
sleepSec :: Int -> IO ()
sleepSec a = sleepSecC (fromIntegral a)
readString :: Int -> IO String
readString a = fmap show (readStringC (fromIntegral a))
readInt :: Int -> IO Int
readInt a = fmap fromIntegral (readIntC (fromIntegral a))
readNum :: Int -> IO Double
readNum a = (readNumC (fromIntegral a)) >>= (\(CDouble x) -> return x)
writeString :: String -> Int -> IO ()
writeString a b = (newCString a) >>= (\x -> writeStringC x (fromIntegral b))
writeInt :: Int -> Int -> IO ()
writeInt a b = writeIntC (fromIntegral a) (fromIntegral b)
writeNum :: Double -> Int -> IO ()
writeNum a b = writeNumC (CDouble a) (fromIntegral b)

data MainABC = MainA Int | MainB Double | MainC String deriving (Show)
mainA :: [String]
mainA = ["a.out"] -- ,"a.out","a.out"]
mainB :: [[MainABC]]
mainB = [[MainA 0, MainB 0.1, MainC "zero"]] --,[MainA 1, MainB 1.1, MainC "one"],[MainA 2, MainB 2.1, MainC "two"]]
mainC :: [[MainABC]]
mainC = replicate mainD [MainA (negate 1), MainB 0.1, MainC "zero"]
mainD :: Int
mainD = 1 -- 3

readMain :: MainABC -> Int -> IO MainABC
readMain (MainA _) a = fmap MainA (readInt a)
readMain (MainB _) a = fmap MainB (readNum a)
readMain (MainC _) a = fmap MainC (readString a)

writeMain :: MainABC -> Int -> IO ()
writeMain (MainA a) b = writeInt a b
writeMain (MainB a) b = writeNum a b
writeMain (MainC a) b = writeString a b

compMain :: MainABC -> MainABC -> Bool
compMain (MainA a) (MainA b) = a == b
compMain (MainB a) (MainB b) = a == b
compMain (MainC a) (MainC b) = a == b
compMain _ _ = False

main :: IO ()
main = getArgs >>= mainS -- mainF

mainS :: [String] -> IO ()
mainS [] = do
 print "hub start"
 mainFF mainA
 mainFG 0 mainB
 actual <- mainFH mainC
 mainFI actual mainB
 print ("hub done " ++ (show actual))
mainS [a,b] = do
 print "spoke start"
 pipeInit a b
 mainFK (head mainC)
 print "spoke done"

mainF :: [String] -> IO ()
mainF [] = do
 mainFF mainA -- start processes
 mainFG 0 mainB -- send stimulus
 actual <- mainFH mainC -- collect responses
 mainFI actual mainB -- check responses
 mainFJ 1 mainD -- wait processes
mainF [a,b] = do
 pipeInit a b
 mainFK (head mainC) -- copy request to response in order given
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

mainFH :: [[MainABC]] -> IO [[MainABC]]
mainFH a = do
 index <- waitAny
 print ("mainFH got index " ++ (show index))
 mainFHF a index

mainFHF :: [[MainABC]] -> Int -> IO [[MainABC]]
mainFHF a b
 | b == mainD = return a
 | otherwise = do
  pattern <- mainFHJ a b
  value <- readMain pattern b
  valid <- checkRead b
  print ("mainFHF " ++ (show valid) ++ " " ++ (show value))
  mainFHG a b value valid

mainFHG :: [[MainABC]] -> Int -> MainABC -> Int -> IO [[MainABC]]
mainFHG a b c 1 = do
 remove <- mainFHH a b
 append <- mainFHI c remove b
 print ("mainFHG append " ++ (show append))
 mainFH append
mainFHG a _ _ 0 = return a

--mainFHH pop value at given index
mainFHH :: [[MainABC]] -> Int -> IO [[MainABC]]
mainFHH a b = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in return (pre ++ [(tail mid)] ++ post)

--mainFHI push value at given index
mainFHI :: MainABC -> [[MainABC]] -> Int -> IO [[MainABC]]
mainFHI a b c = let
 pre = take c b
 rest = drop c b
 post = tail rest
 mid = head rest
 in return (pre ++ [(mid ++ [a])] ++ post)

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
 | otherwise = print ("mismatch " ++ (show a) ++ (show c))
mainFI _ _ = print "impossible"

mainFIF :: [MainABC] -> [MainABC] -> Bool
mainFIF [] [] = True
mainFIF (a:b) (c:d)
 | compMain a c = mainFIF b d
 | otherwise = False
mainFIF _ _ = False

mainFJ :: Int -> Int -> IO ()
mainFJ a b
 | (a == mainD) && (b == mainD) = return ()
 | b == mainD = waitAny >>= (mainFJ (a+1))
 | otherwise = undefined

mainFK :: [MainABC] -> IO ()
mainFK [] = return ()
mainFK (a:b) = do
 index <- waitAny
 print ("mainFK got index " ++ (show index))
 value <- readMain a index
 print ("mainFK got value " ++ (show value))
 writeMain value index
 mainFK b
