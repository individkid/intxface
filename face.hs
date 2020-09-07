--
--    face.hs
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

module Face where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.IORef
import System.Environment
import System.Exit

foreign import ccall "wrapper" wrapJump :: (CString -> CInt -> CInt -> IO ()) -> IO (FunPtr (CString -> CInt -> CInt -> IO ()))
foreign import ccall "readNote" readNoteC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "readJump" readJumpC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "writeJump" writeJumpC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "bothJump" bothJumpC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "closeIdent" closeIdentC :: CInt -> IO ()
foreign import ccall "moveIdent" moveIdentC :: CInt -> CInt -> IO ()
foreign import ccall "openPipe" openPipeC :: IO CInt
foreign import ccall "openFifo" openFifoC :: CString -> IO CInt
foreign import ccall "openFile" openFileC :: CString -> IO CInt
foreign import ccall "forkExec" forkExecC :: CString -> IO CInt
foreign import ccall "pipeInit" pipeInitC :: CString -> CString -> IO CInt
foreign import ccall "waitAny" waitAnyC :: IO CInt
foreign import ccall "pauseAny" pauseAnyC :: CDouble -> IO CInt
foreign import ccall "pollPipe" pollPipeC :: CInt -> IO CInt
foreign import ccall "pollFile" pollFileC :: CInt -> IO CInt
foreign import ccall "seekFile" seekFileC :: CLLong -> CInt -> IO ()
foreign import ccall "truncFile" truncFileC :: CInt -> IO ()
foreign import ccall "checkFile" checkFileC :: CInt -> IO CLLong
foreign import ccall "rdlkFile" rdlkFileC :: CLLong -> CLLong -> CInt -> IO CInt
foreign import ccall "wrlkFile" wrlkFileC :: CLLong -> CLLong -> CInt -> IO CInt
foreign import ccall "unlkFile" unlkFileC :: CLLong -> CLLong -> CInt -> IO () 
foreign import ccall "rdlkwFile" rdlkwFileC :: CLLong -> CLLong -> CInt -> IO () 
foreign import ccall "wrlkwFile" wrlkwFileC :: CLLong -> CLLong -> CInt -> IO ()
foreign import ccall "checkRead" checkReadC :: CInt -> IO CInt
foreign import ccall "checkWrite" checkWriteC :: CInt -> IO CInt
foreign import ccall "sleepSec" sleepSecC :: CInt -> IO ()
foreign import ccall "wrapper" wrapStr :: (CString -> CInt -> IO ()) -> IO (FunPtr (CString -> CInt -> IO ()))
foreign import ccall "readStrHs" readStrC :: FunPtr (CString -> CInt -> IO ()) -> CInt -> IO ()
foreign import ccall "readChr" readChrC :: CInt -> IO CChar
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNew" readNewC :: CInt -> IO CLLong
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "readOld" readOldC :: CInt -> IO CFloat
foreign import ccall "writeStr" writeStrC :: CString -> CInt -> CInt -> IO ()
foreign import ccall "writeChr" writeChrC :: CChar -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeNew" writeNewC :: CLLong -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: CDouble -> CInt -> IO ()
foreign import ccall "writeOld" writeOldC :: CFloat -> CInt -> IO ()

readNote :: (String -> Int -> Int -> IO ()) -> Int -> IO ()
readNote a b = (wrapJump (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= (\x -> readNoteC x (fromIntegral b))
readJump :: (String -> Int -> Int -> IO ()) -> Int -> IO ()
readJump a b = (wrapJump (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= (\x -> readJumpC x (fromIntegral b))
writeJump :: (String -> Int -> Int -> IO ()) -> Int -> IO ()
writeJump a b = (wrapJump (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= (\x -> readJumpC x (fromIntegral b))
bothJump :: (String -> Int -> Int -> IO ()) -> Int -> IO ()
bothJump a b = (wrapJump (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= (\x -> readJumpC x (fromIntegral b))
closeIdent :: Int -> IO ()
closeIdent a = closeIdentC (fromIntegral a)
moveIdent :: Int -> Int -> IO ()
moveIdent a b = moveIdentC (fromIntegral a) (fromIntegral b)
openPipe :: IO Int
openPipe = fmap fromIntegral openPipeC
openFifo :: String -> IO Int
openFifo a = fmap fromIntegral ((newCString a) >>= openFifoC)
openFile :: String -> IO Int
openFile a = fmap fromIntegral ((newCString a) >>= openFileC)
forkExec :: String -> IO Int
forkExec a = fmap fromIntegral ((newCString a) >>= forkExecC)
pipeInit :: String -> String -> IO Int
pipeInit a b = fmap fromIntegral ((newCString a) >>= (\x -> (newCString b) >>= (pipeInitC x)))
waitAny :: IO Int
waitAny = fmap fromIntegral waitAnyC
pauseAny :: Double -> IO Int
pauseAny a = fmap fromIntegral (pauseAnyC (CDouble a))
pollPipe :: Int -> IO Int
pollPipe a = fmap fromIntegral (pollPipeC (fromIntegral a))
pollFile :: Int -> IO Int
pollFile a = fmap fromIntegral (pollFileC (fromIntegral a))
seekFile :: Integer -> Int -> IO ()
seekFile a b = seekFileC (fromIntegral a) (fromIntegral b)
truncFile :: Int -> IO ()
truncFile a = truncFileC (fromIntegral a)
checkFile :: Int -> IO Integer
checkFile a = fmap fromIntegral (checkFileC (fromIntegral a))
rdlkFile :: Integer -> Integer -> Int -> IO CInt
rdlkFile a b c = fmap fromIntegral (rdlkFileC (fromIntegral a) (fromIntegral b) (fromIntegral c))
wrlkFile :: Integer -> Integer -> Int -> IO CInt
wrlkFile a b c = fmap fromIntegral (wrlkFileC (fromIntegral a) (fromIntegral b) (fromIntegral c))
unlkFile :: Integer -> Integer -> Int -> IO () 
unlkFile a b c = unlkFileC (fromIntegral a) (fromIntegral b) (fromIntegral c)
rdlkwFile :: Integer -> Integer -> Int -> IO () 
rdlkwFile a b c = rdlkwFileC (fromIntegral a) (fromIntegral b) (fromIntegral c)
wrlkwFile :: Integer -> Integer -> Int -> IO ()
wrlkwFile a b c = wrlkwFileC (fromIntegral a) (fromIntegral b) (fromIntegral c)
checkRead :: Int -> IO Int
checkRead a = fmap fromIntegral (checkReadC (fromIntegral a))
checkWrite :: Int -> IO Int
checkWrite a = fmap fromIntegral (checkWriteC (fromIntegral a))
sleepSec :: Int -> IO ()
sleepSec a = sleepSecC (fromIntegral a)
readStrF :: IORef (String,Bool) -> CString -> CInt -> IO ()
readStrF a b c = (peekCString b) >>= (\x -> (writeIORef a (x,((fromIntegral c) /= 0))))
readStr :: Int -> IO (String,Bool)
readStr a = do
 b <- newIORef ("",False)
 c <- wrapStr (readStrF b)
 readStrC c (fromIntegral a)
 readIORef b
readChr :: Int -> IO Char
readChr a = fmap castCCharToChar (readChrC (fromIntegral a))
readInt :: Int -> IO Int
readInt a = fmap fromIntegral (readIntC (fromIntegral a))
readNew :: Int -> IO Integer
readNew a = fmap toInteger (readNewC (fromIntegral a))
readNum :: Int -> IO Double
readNum a = (readNumC (fromIntegral a)) >>= (\(CDouble x) -> return x)
readOld :: Int -> IO Float
readOld a = (readOldC (fromIntegral a)) >>= (\(CFloat x) -> return x)
writeStr :: String -> Bool -> Int -> IO ()
writeStr a False b = (newCString a) >>= (\x -> writeStrC x (fromIntegral 0) (fromIntegral b))
writeStr a True b = (newCString a) >>= (\x -> writeStrC x (fromIntegral 1) (fromIntegral b))
writeChr :: Char -> Int -> IO ()
writeChr a b = writeChrC (castCharToCChar a) (fromIntegral b)
writeInt :: Int -> Int -> IO ()
writeInt a b = writeIntC (fromIntegral a) (fromIntegral b)
writeNew :: Integer -> Int -> IO ()
writeNew a b = writeNewC (fromInteger a) (fromIntegral b)
writeNum :: Double -> Int -> IO ()
writeNum a b = writeNumC (CDouble a) (fromIntegral b)
writeOld :: Float -> Int -> IO ()
writeOld a b = writeOldC (CFloat a) (fromIntegral b)
