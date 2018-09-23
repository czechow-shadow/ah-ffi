module PCREcompile where

import Protolude

import System.IO.Unsafe
import Foreign
import Foreign.C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Regex


data PCRE
data PCREExtra


foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString
                 -> PCREOption
                 -> Ptr CString
                 -> Ptr CInt
                 -> Ptr Word8
                 -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !ByteString
           deriving (Eq, Ord, Show)

{-
pcre *pcre_compile(const char *pattern,
                   int options,
                   const char **errptr,
                   int *erroffset,
                   const unsigned char *tableptr);
-}
compile :: ByteString -> [PCREOption] -> Either Text Regex
compile str opts = unsafePerformIO $ do
  BS.useAsCString str $ \pattern' -> do
    alloca $ \errptr -> do
      alloca $ \erroffset -> do
        res <- c_pcre_compile pattern' (combineOptions opts) errptr erroffset nullPtr
        if res == nullPtr
          then do ep <- peek errptr
                  e <- peekCString ep
                  pure $ Left $ toS e
          else do reg <- newForeignPtr finalizerFree res
          --else do reg <- newForeignPtr ok_free res
          --else do reg <- newForeignPtr pcre_free res
                  pure $ Right $ Regex reg str


-- why does it fail with core dump...
foreign import ccall "pcre.h &pcre_free"
  pcre_free :: FunPtr (Ptr a -> IO ())

-- but this one is fine?
foreign import ccall "mymath.h &okfree"
  ok_free :: FunPtr (Ptr a -> IO ())

foreign import ccall "mymath.h &wrong_free"
  wrong_free :: FunPtr (Ptr a -> IO ())
  
{-
int pcre_exec(const pcre *code,
              const pcre_extra *extra,
              const char *subject,
              int length,
              int startoffset,
              int options,
              int *ovector,
              int ovecsize);
-} 
foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE
              -> Ptr PCREExtra
              -> CString
              -> CInt
              -> CInt
              -> PCREExecOption
              -> Ptr CInt
              -> CInt
              -> IO CInt
  

{-
int pcre_fullinfo( const pcre *code
                 , const pcre_extra *extra
                 , int what
                 , void *where);
-}
foreign import ccall unsafe "pcre.h pcre_fullinfo"
  c_pcre_fullinfo :: Ptr PCRE
                  -> Ptr PCREExtra
                  -> PCREInfo
                  -> Ptr a
                  -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
  alloca $ \n_ptr -> do
    res <- c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
    if res == 0
      then do n <- (peek n_ptr :: IO CInt)
              pure $ fromIntegral n
      else panic "capturedcount failed"
              
      
match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject opts = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (0 :: CInt)
    allocaBytes ovec_bytes $ \ovec -> do
      BS.unsafeUseAsCString subject $ \cstr -> do
        r <- c_pcre_exec pcre_ptr
                         nullPtr
                         cstr
                         (fromIntegral $ BS.length subject)
                         0
                         (combineExecOptions opts)
                         ovec
                         (fromIntegral ovec_size)
        if r < 0
          then pure Nothing
          else let loop n o acc =
                     if n == r
                     then return $ Just (reverse acc)
                     else do
                       i <- peekElemOff ovec o
                       j <- peekElemOff ovec (o+1)
                       let s = substring i j subject
                       loop (n+1) (o+2) (s : acc)
               in loop 0 0 []
  where
    substring :: CInt -> CInt -> ByteString -> ByteString
    substring x y _ | x == y = BS.empty
    substring a b s = end
        where
            start = BS.unsafeDrop (fromIntegral a) s
            end   = BS.unsafeTake (fromIntegral (b-a)) start  

  
