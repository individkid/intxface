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
foreign import ccall "pipeInit" pipeInit :: CString -> CString -> IO ()
foreign import ccall "waitAny" waitAnyC :: IO CInt
foreign import ccall "sleepSec" sleepSecC :: CInt -> IO ()
foreign import ccall "readString" readStringC :: CInt -> IO CString
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "writeString" writeStringC :: CString -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: CDouble -> CInt -> IO ()

forkExec :: String -> IO ()
forkExec a = (newCString a) >>= forkExecC
waitAny :: IO Int
waitAny = fmap fromIntegral waitAny
readInt :: Int -> IO Int
readInt a = fmap fromIntegral (readIntC (fromIntegral a))
readNum :: Int -> IO Double
readNum a = (readNumC (fromIntegral a)) >>= (\(CDouble x) -> return x)
readString :: Int -> IO String
readString a = fmap show (readStringC (fromIntegral a))
writeInt :: Int -> Int -> IO ()
writeInt a b = writeIntC (fromIntegral a) (fromIntegral b)
writeNum :: Double -> Int -> IO ()
writeNum a b = writeNumC (CDouble a) (fromIntegral b)
writeString a b = (newCString a) >>= (\x -> writeStringC x (fromIntegral b))

data MainABC = MainA Int | MainB Double | MainC String
mainA :: [String]
mainA = ["a.out","a.out","a.out"]
mainB :: [[MainABC]]
mainB = [[MainA 0, MainB 0.1, MainC "zero"],[MainA 1, MainB 1.1, MainC "one"],[MainA 2, MainB 2.1, MainC "two"]]
mainC :: [[MainABC]]
mainC = replicate 3 [MainA (negate 1), MainB 0.0, MainC ""]
mainD :: Int
mainD = 3

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
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [] = do
 mainFF mainA -- start processes
 mainFG 0 mainB -- send stimulus
 actual <- (mainFH (replicate mainD []) mainC) -- collect responses
 mainFI actual mainB -- check responses
 print "hub passed"
mainF [av1,av2] = do
 argv1 <- newCString av1
 argv2 <- newCString av2
 pipeInit argv1 argv2
 mainFJ (head mainC) -- copy request to response in order given
 print "spoke passed"
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
mainFH a b = waitAny >>= (mainFHF a b 1)

mainFHF :: [[MainABC]] -> [[MainABC]] -> Int -> Int -> IO [[MainABC]]
mainFHF a b c d
 | (c == mainD) && (d == mainD) = return a
 | d == mainD = waitAny >>= (mainFHF a b (c+1))
 | otherwise = (readMain (mainFHG b d) d) >>= (\x -> mainFH (mainFHH x a d) (mainFHI b d))

--mainFHG return head value at given index
mainFHG :: [[MainABC]] -> Int -> MainABC
mainFHG a b = head (head (drop b a))

--mainFHH append value at given index
mainFHH :: MainABC -> [[MainABC]] -> Int -> [[MainABC]]
mainFHH a b c = let
 pre = take c b
 rest = drop c b
 post = tail rest
 mid = head rest
 in pre ++ [(mid ++ [a])] ++ post

--mainFHI remove head at given index
mainFHI :: [[MainABC]] -> Int -> [[MainABC]]
mainFHI a b = let
 pre = take b a
 rest = drop b a
 post = tail rest
 mid = head rest
 in pre ++ [(tail mid)] ++ post

mainFI :: [[MainABC]] -> [[MainABC]] -> IO ()
mainFI (a:b) (c:d)
 | mainFIF a c = mainFI b d
 | otherwise = undefined

mainFIF :: [MainABC] -> [MainABC] -> Bool
mainFIF [] [] = True
mainFIF (a:b) (c:d)
 | compMain a c = mainFIF b d
 | otherwise = False
mainFIF _ _ = False

mainFJ :: [MainABC] -> IO ()
mainFJ [] = return ()
mainFJ (a:b) = do
 index <- waitAny
 value <- readMain a index
 writeMain value index
 mainFJ b
