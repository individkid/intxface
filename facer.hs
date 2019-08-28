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

foreign import ccall "forkExec" forkExec :: CString -> IO ()
foreign import ccall "pipeInit" pipeInit :: CString -> CString -> IO ()
foreign import ccall "waitAny" waitAny :: IO CInt
foreign import ccall "sleepSec" sleepSec :: CInt -> IO ()
foreign import ccall "readString" readString :: CInt -> IO CString
foreign import ccall "readInt" readInt :: CInt -> IO CInt
foreign import ccall "readNum" readNum :: CInt -> IO CDouble
foreign import ccall "writeString" writeString :: CString -> CInt -> IO ()
foreign import ccall "writeInt" writeInt :: CInt -> CInt -> IO ()
foreign import ccall "writeNum" writeNum :: CDouble -> CInt -> IO ()

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [av1,av2] = do
 argv1 <- newCString av1
 argv2 <- newCString av2
 pipeInit argv1 argv2
 index <- waitAny
 response <- readInt index
 sleepSec(1)
 writeInt response index
 print "spoke passed"
mainF [] = do
 (newCString "a.out") >>= forkExec
 (newCString "a.out") >>= forkExec
 (newCString "a.out") >>= forkExec
 writeInt 0 0
 writeInt 1 1
 writeInt 2 2
 waitAny >>= readInt >>= print
 waitAny >>= readInt >>= print
 waitAny >>= readInt >>= print
 waitAny
 waitAny
 waitAny
 print "hub passed"
mainF _ = undefined
