{-# LANGUAGE ForeignFunctionInterface #-}

module Face where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.IORef
import System.Environment
import System.Exit

foreign import ccall "wrapper" wrapNote :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" wrapErr :: (CString -> CInt -> CInt -> IO ()) -> IO (FunPtr (CString -> CInt -> CInt -> IO ()))
foreign import ccall "wrapper" wrapStr :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "wrapper" wrapInt :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" wrapNum :: (CDouble -> IO ()) -> IO (FunPtr (CDouble -> IO ()))
foreign import ccall "wrapper" wrapNew :: (CLLong -> IO ()) -> IO (FunPtr (CLLong -> IO ()))
foreign import ccall "wrapper" wrapOld :: (CFloat -> IO ()) -> IO (FunPtr (CFloat -> IO ()))

wrapStrF :: IORef String -> CString -> IO ()
wrapStrF a b = (peekCString b) >>= (writeIORef a)
wrapIntF :: IORef Int -> CInt -> IO ()
wrapIntF a b = writeIORef a (fromIntegral b)
wrapNumF :: IORef Double -> CDouble -> IO ()
wrapNumF a (CDouble b) = writeIORef a b
wrapNewF :: IORef Integer -> CLLong -> IO ()
wrapNewF a b = writeIORef a (fromIntegral b)
wrapOldF :: IORef Float -> CFloat -> IO ()
wrapOldF a (CFloat b) = writeIORef a b

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
foreign import ccall "readStrHs" readStrC :: FunPtr (CString -> IO ()) -> CInt -> IO ()
foreign import ccall "readChr" readChrC :: CInt -> IO CChar
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readNew" readNewC :: CInt -> IO CLLong
foreign import ccall "readNum" readNumC :: CInt -> IO CDouble
foreign import ccall "readOld" readOldC :: CInt -> IO CFloat
foreign import ccall "writeStr" writeStrC :: CString -> CInt -> IO ()
foreign import ccall "writeChr" writeChrC :: CChar -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeNew" writeNewC :: CLLong -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: CDouble -> CInt -> IO ()
foreign import ccall "writeOld" writeOldC :: CFloat -> CInt -> IO ()

foreign import ccall "hideEnumHs" hideEnumC :: CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideOpenHs" hideOpenC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideCloseHs" hideCloseC :: CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideFieldHs" hideFieldC :: CString -> CInt -> Ptr CInt -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideStrHs" hideStrC :: FunPtr (CString -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideIntHs" hideIntC :: FunPtr (CInt -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideNumHs" hideNumC :: FunPtr (CDouble -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideNewHs" hideNewC :: FunPtr (CLLong -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideOldHs" hideOldC :: FunPtr (CFloat -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int

foreign import ccall "showEnumHs" showEnumC :: CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showOpenHs" showOpenC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showCloseHs" showCloseC :: CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showFieldHs" showFieldC :: CString -> CInt -> Ptr CInt -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showStrHs" showStrC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showIntHs" showIntC :: CInt -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showNumHs" showNumC :: CDouble -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showNewHs" showNewC :: CLLong -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showOldHs" showOldC :: CFloat -> CString -> FunPtr (CString -> IO ()) -> IO ()

errFunc :: (String -> Int -> Int -> IO ()) -> IO ()
errFunc a = (wrapErr (\x y z -> (peekCString x) >>= (\x -> a x (fromIntegral y) (fromIntegral z)))) >>= errFuncC
noteFunc :: (Int -> IO ()) -> IO ()
noteFunc a = (wrapNote (\x -> a (fromIntegral x))) >>= noteFuncC
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
callInit a b = (wrapInt (\x -> a (fromIntegral x))) >>= (\x -> callInitC x (fromIntegral b))
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

readStr :: Int -> IO String
readStr a = do
 b <- newIORef ""
 c <- wrapStr (wrapStrF b)
 readStrC c (fromIntegral a)
 readIORef b
readDat :: Int -> IO [Char]
readDat a = undefined
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
writeDat :: [Char] -> Int -> IO ()
writeDat a b = undefined
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

hideEnum :: String -> String -> IORef String -> IO Bool
hideEnum a b c = do
 ax <- newCString a
 bx <- newCString b
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 x <- hideEnumC ax bx cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return False
  _ -> return True
hideOpen :: String -> IORef String -> IO Bool
hideOpen a c = do
 ax <- newCString a
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 x <- hideOpenC ax cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return False
  _ -> return True
hideClose :: IORef String -> IO Bool
hideClose c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 x <- hideCloseC cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return False
  _ -> return True
hideField :: String -> [Int] -> IORef String -> IO Bool
hideField a b c = do
 ax <- newCString a
 bx <- newArray (map fromIntegral b)
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 x <- hideFieldC ax (fromIntegral (length b)) bx cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return False
  _ -> return True
hideType :: (IORef a -> CString -> FunPtr (CString -> IO ()) -> IO Int) -> a -> IORef String -> IO (Maybe a)
hideType i f c = do
 cx <- readIORef c >>= newCString -- src string
 d <- newIORef "" -- dst string ref
 dx <- wrapStr (wrapStrF d)
 e <- newIORef f -- val ref
 x <- i e cx dx
 readIORef d >>= writeIORef c
 case x of
  0 -> return Nothing
  1 -> fmap Just (readIORef e)
x0 :: (x -> b) -> (a -> x) -> a -> b
x0 f g a = f (g a)
y2 :: (y -> b -> c -> IO d) -> (a -> IO y) -> a -> b -> c -> IO d
y2 f g a c d = do
 b <- g a
 f b c d
hideDat :: IORef String -> IO (Maybe [Char])
hideDat = undefined
hideStr :: IORef String -> IO (Maybe String)
hideStr = hideType (y2 hideStrC (x0 wrapStr wrapStrF)) ""
hideInt :: IORef String -> IO (Maybe Int)
hideInt = hideType (y2 hideIntC (x0 wrapInt wrapIntF)) 0
hideNum :: IORef String -> IO (Maybe Double)
hideNum = hideType (y2 hideNumC (x0 wrapNum wrapNumF)) 0
hideNew :: IORef String -> IO (Maybe Integer)
hideNew = hideType (y2 hideNewC (x0 wrapNew wrapNewF)) 0
hideOld :: IORef String -> IO (Maybe Float)
hideOld = hideType (y2 hideOldC (x0 wrapOld wrapOldF)) 0

showEnum :: String -> String -> IORef String -> IO ()
showEnum a b c = do
 ax <- newCString a
 bx <- newCString b
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 showEnumC ax bx cx dx
 readIORef d >>= writeIORef c
showOpen :: String -> IORef String -> IO ()
showOpen a c = do
 ax <- newCString a
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 showOpenC ax cx dx
 readIORef d >>= writeIORef c
showClose :: IORef String -> IO ()
showClose c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 showCloseC cx dx
 readIORef d >>= writeIORef c
showField :: String -> [Int] -> IORef String -> IO ()
showField a b c = do
 ax <- newCString a
 bx <- newArray (map fromIntegral b)
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (wrapStrF d)
 showFieldC ax (fromIntegral (length b)) bx cx dx
showDat :: [Char] -> IORef String -> IO ()
showDat a b = undefined
showStr :: String -> IORef String -> IO ()
showStr a c = undefined
showInt :: Int -> IORef String -> IO ()
showInt a c = undefined
showNum :: Double -> IORef String -> IO ()
showNum a c = undefined
showNew :: Integer -> IORef String -> IO ()
showNew a c = undefined
showOld :: Float -> IORef String -> IO ()
showOld a c = undefined
