{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Face where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.IORef
import Data.Int
import System.Environment
import System.Exit

foreign import ccall "wrapper" wrapNote :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" wrapErr :: (CString -> CInt -> CInt -> IO ()) -> IO (FunPtr (CString -> CInt -> CInt -> IO ()))
foreign import ccall "wrapper" wrapStr :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "wrapper" wrapDat :: (CInt -> Ptr CChar -> IO ()) -> IO (FunPtr (CInt -> Ptr CChar -> IO ()))
foreign import ccall "wrapper" wrapInt :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" wrapInt32 :: (Int32 -> IO ()) -> IO (FunPtr (Int32 -> IO ()))
foreign import ccall "wrapper" wrapNum :: (Double -> IO ()) -> IO (FunPtr (Double -> IO ()))
foreign import ccall "wrapper" wrapNew :: (CLLong -> IO ()) -> IO (FunPtr (CLLong -> IO ()))
foreign import ccall "wrapper" wrapOld :: (Float -> IO ()) -> IO (FunPtr (Float -> IO ()))

foreign import ccall "noteFunc" noteFuncC :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall "errFunc" errFuncC :: FunPtr (CString -> CInt -> CInt -> IO ()) -> IO ()
foreign import ccall "closeIdent" closeIdentC :: CInt -> IO ()
foreign import ccall "moveIdent" moveIdentC :: CInt -> CInt -> IO ()
foreign import ccall "openPipe" openPipeC :: IO CInt
foreign import ccall "openFifo" openFifoC :: CString -> IO CInt
foreign import ccall "openFile" openFileC :: CString -> IO CInt
foreign import ccall "forkExec" forkExecC :: CString -> IO CInt
foreign import ccall "pipeInit" pipeInitC :: CString -> CString -> IO CInt
foreign import ccall "openFork" openForkC :: IO CInt
foreign import ccall "openCheck" openCheckC :: CInt -> IO CInt
foreign import ccall "openRdfd" openRdfd :: CInt -> IO CInt
foreign import ccall "openWrfd" openWrfd :: CInt -> IO CInt
foreign import ccall "openExec" openExecC :: CString -> CString -> IO CInt
foreign import ccall "rdwrInit" rdwrInitC :: CInt -> CInt -> IO CInt
foreign import ccall "waitRead" waitReadC :: Double -> CInt -> IO CInt
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
foreign import ccall "readDatHs" readDatC :: FunPtr (CInt -> Ptr CChar -> IO ()) -> CInt -> IO ()
foreign import ccall "readChr" readChrC :: CInt -> IO CChar
foreign import ccall "readInt" readIntC :: CInt -> IO CInt
foreign import ccall "readInt32" readInt32C :: CInt -> IO Int32
foreign import ccall "readNew" readNewC :: CInt -> IO CLLong
foreign import ccall "readNum" readNumC :: CInt -> IO Double
foreign import ccall "readOld" readOldC :: CInt -> IO Float
foreign import ccall "writeStr" writeStrC :: CString -> CInt -> IO ()
foreign import ccall "writeDatHs" writeDatC :: CInt -> Ptr CChar -> CInt -> IO ()
foreign import ccall "writeChr" writeChrC :: CChar -> CInt -> IO ()
foreign import ccall "writeInt" writeIntC :: CInt -> CInt -> IO ()
foreign import ccall "writeInt32" writeInt32C :: Int32 -> CInt -> IO ()
foreign import ccall "writeNew" writeNewC :: CLLong -> CInt -> IO ()
foreign import ccall "writeNum" writeNumC :: Double -> CInt -> IO ()
foreign import ccall "writeOld" writeOldC :: Float -> CInt -> IO ()

foreign import ccall "hideEnumHs" hideEnumC :: CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideOpenHs" hideOpenC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideCloseHs" hideCloseC :: CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideFieldHs" hideFieldC :: CString -> CInt -> Ptr CInt -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideStrHs" hideStrC :: FunPtr (CString -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideDatHs" hideDatC :: FunPtr (CInt -> Ptr CChar -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideIntHs" hideIntC :: FunPtr (CInt -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideInt32Hs" hideInt32C :: FunPtr (Int32 -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideNumHs" hideNumC :: FunPtr (Double -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideNewHs" hideNewC :: FunPtr (CLLong -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
foreign import ccall "hideOldHs" hideOldC :: FunPtr (Float -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int

foreign import ccall "showEnumHs" showEnumC :: CString -> CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showOpenHs" showOpenC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showCloseHs" showCloseC :: CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showFieldHs" showFieldC :: CString -> CInt -> Ptr CInt -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showStrHs" showStrC :: CString -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showDatHs" showDatC :: CInt -> Ptr CChar -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showIntHs" showIntC :: CInt -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showInt32Hs" showInt32C :: Int32 -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showNumHs" showNumC :: Double -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showNewHs" showNewC :: CLLong -> CString -> FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "showOldHs" showOldC :: Float -> CString -> FunPtr (CString -> IO ()) -> IO ()

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
waitRead a b = fmap fromIntegral (waitReadC a (fromIntegral b))
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
 c <- wrapStr (\x -> (peekCString x) >>= (writeIORef b))
 readStrC c (fromIntegral a)
 readIORef b
readDat :: Int -> IO [CChar]
readDat a = do
 b <- newIORef []
 c <- wrapDat (\x y -> (peekArray (fromIntegral x) y) >>= (writeIORef b))
 readDatC c (fromIntegral a)
 readIORef b
readChr :: Int -> IO Char
readChr a = fmap castCCharToChar (readChrC (fromIntegral a))
readInt :: Int -> IO Int
readInt a = fmap fromIntegral (readIntC (fromIntegral a))
readInt32 :: Int -> IO Int32
readInt32 a = readInt32C (fromIntegral a)
readNew :: Int -> IO Integer
readNew a = fmap toInteger (readNewC (fromIntegral a))
readNum :: Int -> IO Double
readNum a = readNumC (fromIntegral a)
readOld :: Int -> IO Float
readOld a = readOldC (fromIntegral a)

writeStr :: String -> Int -> IO ()
writeStr a b = (newCString a) >>= (\x -> writeStrC x (fromIntegral b))
writeDat :: [CChar] -> Int -> IO ()
writeDat a b = newArray a >>= (\x -> writeDatC (fromIntegral (length a)) x (fromIntegral b))
writeChr :: Char -> Int -> IO ()
writeChr a b = writeChrC (castCharToCChar a) (fromIntegral b)
writeInt :: Int -> Int -> IO ()
writeInt a b = writeIntC (fromIntegral a) (fromIntegral b)
writeInt32 :: Int32 -> Int -> IO ()
writeInt32 a b = writeInt32C a (fromIntegral b)
writeNew :: Integer -> Int -> IO ()
writeNew a b = writeNewC (fromInteger a) (fromIntegral b)
writeNum :: Double -> Int -> IO ()
writeNum a b = writeNumC a (fromIntegral b)
writeOld :: Float -> Int -> IO ()
writeOld a b = writeOldC a (fromIntegral b)

class StrIO a where
 choose :: IO a
 wrap :: (a -> IO ()) -> IO (FunPtr (a -> IO()))
 hide :: FunPtr (a -> IO ()) -> CString -> FunPtr (CString -> IO ()) -> IO Int
 showt :: a -> CString -> FunPtr (CString -> IO ()) -> IO ()

instance StrIO CString where
 choose = newCString ""
 wrap = wrapStr
 hide = hideStrC
 showt = showStrC
instance StrIO CInt where
 choose = return 0
 wrap = wrapInt
 hide = hideIntC
 showt = showIntC
instance StrIO Int32 where
 choose = return 0
 wrap = wrapInt32
 hide = hideInt32C
 showt = showInt32C
instance StrIO Double where
 choose = return 0
 wrap = wrapNum
 hide = hideNumC
 showt = showNumC
instance StrIO CLLong where
 choose = return 0
 wrap = wrapNew
 hide = hideNewC
 showt = showNewC
instance StrIO Float where
 choose = return 0
 wrap = wrapOld
 hide = hideOldC
 showt = showOldC

hideType :: StrIO a => IORef String -> IO (Maybe a)
hideType c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 e <- choose >>= newIORef
 ex <- wrap (\b -> writeIORef e b)
 x <- hide ex cx dx
 readIORef d >>= writeIORef c
 case x of
  0 -> return Nothing
  1 -> fmap Just (readIORef e)

showType :: StrIO a => a -> IORef String -> IO ()
showType a c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 showt a cx dx
 readIORef d >>= writeIORef c

hideStrF :: Maybe CString -> IO (Maybe String)
hideStrF Nothing = return Nothing
hideStrF (Just a) = peekCString a >>= return . Just
hideIntF :: Maybe CInt -> IO (Maybe Int)
hideIntF = return . fmap fromIntegral
hideNewF :: Maybe CLLong -> IO (Maybe Integer)
hideNewF = return . fmap fromIntegral

hideDat :: IORef String -> IO (Maybe [CChar])
hideDat = undefined
hideStr :: IORef String -> IO (Maybe String)
hideStr a = hideType a >>= hideStrF
hideInt :: IORef String -> IO (Maybe Int)
hideInt a = hideType a >>= hideIntF
hideInt32 :: IORef String -> IO (Maybe Int32)
hideInt32 a = hideType a
hideNum :: IORef String -> IO (Maybe Double)
hideNum a = hideType a
hideNew :: IORef String -> IO (Maybe Integer)
hideNew a = hideType a >>= hideNewF
hideOld :: IORef String -> IO (Maybe Float)
hideOld a = hideType a

hideEnum :: String -> String -> IORef String -> IO (Maybe Bool)
hideEnum a b c = do
 ax <- newCString a
 bx <- newCString b
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 x <- hideEnumC ax bx cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return Nothing
  _ -> return (Just True)
hideOpen :: String -> IORef String -> IO (Maybe Bool)
hideOpen a c = do
 ax <- newCString a
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 x <- hideOpenC ax cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return Nothing
  _ -> return (Just True)
hideClose :: IORef String -> IO (Maybe Bool)
hideClose c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 x <- hideCloseC cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return Nothing
  _ -> return (Just True)
hideFieldF :: String -> [Int] -> IORef String -> IO (Maybe Bool)
hideFieldF a b c = do
 ax <- newCString a
 bx <- newArray (map fromIntegral b)
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 x <- hideFieldC ax (fromIntegral (length b)) bx cx dx
 readIORef d >>= writeIORef c
 case (fromIntegral x) of
  0 -> return Nothing
  _ -> return (Just True)
hideFieldG :: String -> Maybe [Int] -> IORef String -> IO (Maybe Bool)
hideFieldG _ Nothing _ = return Nothing
hideFieldG a (Just b) c = hideFieldF a b c
hideField :: String -> [Maybe Int] -> IORef String -> IO (Maybe Bool)
hideField a b c = hideFieldG a (sequence b) c

showStrF :: String -> IO CString
showStrF a = newCString a
showIntF :: Int -> IO CInt
showIntF a = return (fromIntegral a)
showNewF :: Integer -> IO CLLong
showNewF a = return (fromIntegral a)

showDat :: [CChar] -> IORef String -> IO ()
showDat = undefined
showStr :: String -> IORef String -> IO ()
showStr a b = showStrF a >>= (\x -> showType x b)
showInt :: Int -> IORef String -> IO ()
showInt a b = showIntF a >>= (\x -> showType x b)
showInt32 :: Int32 -> IORef String -> IO ()
showInt32 a b = showType a b
showNum :: Double -> IORef String -> IO ()
showNum a b = showType a b
showNew :: Integer -> IORef String -> IO ()
showNew a b = showNewF a >>= (\x -> showType x b)
showOld :: Float -> IORef String -> IO ()
showOld a b = showType a b

showEnum :: String -> String -> IORef String -> IO ()
showEnum a b c = do
 ax <- newCString a
 bx <- newCString b
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 showEnumC ax bx cx dx
 readIORef d >>= writeIORef c
showOpen :: String -> IORef String -> IO ()
showOpen a c = do
 ax <- newCString a
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 showOpenC ax cx dx
 readIORef d >>= writeIORef c
showClose :: IORef String -> IO ()
showClose c = do
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 showCloseC cx dx
 readIORef d >>= writeIORef c
showField :: String -> [Int] -> IORef String -> IO ()
showField a b c = do
 ax <- newCString a
 bx <- newArray (map fromIntegral b)
 cx <- readIORef c >>= newCString
 d <- newIORef ""
 dx <- wrapStr (\x -> (peekCString x) >>= (writeIORef d))
 showFieldC ax (fromIntegral (length b)) bx cx dx
 readIORef d >>= writeIORef c
