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

import Face
import System.Environment
import System.Exit
import Data.IORef

data MainABC = MainA Int | MainB Double | MainC String | MainD Integer | MainE Float deriving (Show,Eq)
mainA :: [String]
mainA = ["a.out","b.out","facer.ex"]
mainB :: [[MainABC]]
mainB = [[MainA 0, MainB 0.1, MainC "zero", MainD 10, MainE 0.2],
         [MainA 1, MainB 1.1, MainC "one", MainD 11, MainE 1.2],
         [MainA 2, MainB 2.1, MainC "two", MainD 12, MainE 2.2]]
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
mainCG (MainD a) = MainD (negate 1)
mainCG (MainE a) = MainE 0.0

readMain :: MainABC -> Int -> IO MainABC
readMain (MainA _) a = fmap MainA (readInt a)
readMain (MainB _) a = fmap MainB (readNum a)
readMain (MainC _) a = fmap MainC (readStr a)
readMain (MainD _) a = fmap MainD (readNew a)
readMain (MainE _) a = fmap MainE (readOld a)

writeMain :: MainABC -> Int -> IO ()
writeMain (MainA a) b = writeInt a b
writeMain (MainB a) b = writeNum a b
writeMain (MainC a) b = writeStr a b
writeMain (MainD a) b = writeNew a b
writeMain (MainE a) b = writeOld a b

showMain :: MainABC -> String
showMain (MainA a) = "int "++(show a)
showMain (MainB a) = "num "++(show a)
showMain (MainC a) = "str "++(show a)
showMain (MainD a) = "new "++(show a)
showMain (MainE a) = "old "++(show a)

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [] = do
 mainFF mainA -- start processes
 file <- openFile "oops.txt"
 sleepSec 1
 mainFG 0 mainB -- send stimulus
 mainFH mainC mainB -- check responses
 var <- newIORef 0
 readJump (writeIORef var) file
 writeStr "" file
 seekFile 0 file
 readInt file
 act <- readIORef var
 check <- mainFI 0 [act] -- check processes
 mainFK check
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
 | c < 0 = return ()
 | (length (a !! c)) == 0 =
  (readInt c) >> (mainFH a b)
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

mainFK :: [Int] -> IO ()
mainFK [3,0,0,0,0,0,0] = return ()
mainFK _ = undefined

