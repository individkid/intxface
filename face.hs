{-# LANGUAGE ForeignFunctionInterface #-}

module Face where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.IORef
import System.Environment
import System.Exit

foreign import ccall "wrapper" wrapNoteC :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" wrapErrC :: (CString -> CInt -> CInt -> IO ()) -> IO (FunPtr (CString -> CInt -> CInt -> IO ()))
foreign import ccall "noteFunc" noteFuncC :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall "errFunc" errFuncC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> IO ()
foreign import ccall "closeIdent" closeIdentC :: CInt -> IO ()
foreign import ccall "moveIdent" moveIdentC :: CInt -> CInt -> IO ()
foreign import ccall "openPipe" openPipeC :: IO CInt
foreign import ccall "openFifo" openFifoC :: CString -> IO CInt
foreign import ccall "openFile" openFileC :: CString -> IO CInt
foreign import ccall "forkExec" forkExecC :: CString -> IO CInt
foreign import ccall "pipeInit" pipeInitC :: CString -> CString -> IO CInt
foreign import ccall "waitRead" waitReadC :: CDouble -> CInt -> IO CInt
foreign import ccall "wrapper" wrapCall :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "callInit" callInitC :: FunPtr (CInt -> IO ()) -> CInt -> IO ()
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
foreign import ccall "wrapper" wrapStr :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "readStrHs" readStrC :: FunPtr (CString -> IO ()) -> CInt -> IO ()
foreign import ccall "readDat" readDatC :: CInt -> IO CInt
foreign import ccall "readChr" readChrC :: CInt -> IO CChar
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNew" readNewC :: CInt -> IO CLLong
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "readOld" readOldC :: CInt -> IO CFloat
foreign import ccall "writeStr" writeStrC :: CString -> CInt -> IO ()
foreign import ccall "writeDat" writeDatC :: CInt -> CInt -> IO()
foreign import ccall "writeChr" writeChrC :: CChar -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeNew" writeNewC :: CLLong -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: CDouble -> CInt -> IO ()
foreign import ccall "writeOld" writeOldC :: CFloat -> CInt -> IO ()

errFunc :: (String -> Int -> Int -> IO ()) -> IO ()
errFunc a = (wrapErrC (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= errFuncC
noteFunc :: (Int -> IO ()) -> IO ()
noteFunc a = (wrapNoteC (\x -> a (fromIntegral x))) >>= noteFuncC
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
waitRead :: Double -> Int -> IO Int
waitRead a b = fmap fromIntegral (waitReadC (CDouble a) (fromIntegral b))
callInit :: (Int -> IO ()) -> Int -> IO ()
callInit a b = (wrapCall (\x -> a (fromIntegral x))) >>= (\x -> callInitC x (fromIntegral b))
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
readStrF :: IORef String -> CString -> IO ()
readStrF a b = (peekCString b) >>= (writeIORef a)
readStr :: Int -> IO String
readStr a = do
 b <- newIORef ""
 c <- wrapStr (readStrF b)
 readStrC c (fromIntegral a)
 readIORef b
readDat :: Int -> IO Int
readDat a = fmap fromIntegral (readDatC (fromIntegral a))
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
writeStr :: String -> Int -> IO ()
writeStr a b = (newCString a) >>= (\x -> writeStrC x (fromIntegral b))
writeDat :: Int -> Int -> IO ()
writeDat a b = writeDatC (fromIntegral a) (fromIntegral b)
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
