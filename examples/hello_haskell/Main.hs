{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign
import GHC.Exts

#if defined(wasi_HOST_OS)
foreign import ccall "wasm_minimal_protocol_write_args_to_buffer"
  wasm_minimal_protocol_write_args_to_buffer :: Addr# -> IO ()

foreign import ccall "wasm_minimal_protocol_send_result_to_host"
  wasm_minimal_protocol_send_result_to_host :: Addr# -> Int# -> IO ()
#else
wasm_minimal_protocol_write_args_to_buffer :: Addr# -> IO ()
wasm_minimal_protocol_write_args_to_buffer = 
  error "Word size is not 32, replacing wasm_minimal_protocol function with stub"

wasm_minimal_protocol_send_result_to_host :: Addr# -> Int# -> IO ()
wasm_minimal_protocol_send_result_to_host = 
  error "Word size is not 32, replacing wasm_minimal_protocol function with stub"
#endif

createBuffer :: [Int] -> IO [ByteString]
createBuffer lens = do
  let totalLen = sum lens
  strs <- allocaBytes totalLen $ \ptr@(Ptr addr#) -> do
    wasm_minimal_protocol_write_args_to_buffer addr#
    BS.packCStringLen (ptr, totalLen)
  return $ let offs = scanl (+) 0 lens in [BS.take len $ BS.drop off strs | off <- offs | len <- lens]

sendResultToHost :: ByteString -> IO ()
sendResultToHost bs =
  BS.useAsCStringLen bs $ \(Ptr addr#, I# len#) ->
    wasm_minimal_protocol_send_result_to_host addr# len#

foreign export ccall "hello"
  hello :: IO Int
hello :: IO Int
hello = do
  sendResultToHost "Hello from wasm!!!"
  return 0

foreign export ccall "double_it"
  doubleIt :: Int -> IO Int
doubleIt :: Int -> IO Int
doubleIt len = do
  [bs] <- createBuffer [len]
  sendResultToHost (bs <> bs)
  return 0

foreign export ccall "concatenate"
  concatenate :: Int -> Int -> IO Int
concatenate :: Int -> Int -> IO Int
concatenate len1 len2 = do
  [bs1, bs2] <- createBuffer [len1, len2]
  sendResultToHost (bs1 <> "*" <> bs2)
  return 0

foreign export ccall "shuffle"
  shuffle :: Int -> Int -> Int -> IO Int
shuffle :: Int -> Int -> Int -> IO Int
shuffle len1 len2 len3 = do
  [bs1, bs2, bs3] <- createBuffer [len1, len2, len3]
  let combined = bs3 <> "-" <> bs1 <> "-" <> bs2
  sendResultToHost combined
  return 0

foreign export ccall "returns_ok"
  returnsOk :: IO Int
returnsOk :: IO Int
returnsOk = do
  sendResultToHost "This is an `Ok`"
  return 0

foreign export ccall "will_panic"
  willPanic :: IO Int
willPanic :: IO Int
willPanic = return 1

foreign export ccall "returns_err"
  returnsErr :: IO Int
returnsErr :: IO Int
returnsErr = do
  sendResultToHost "This is an `Err`"
  return 1

main :: IO ()
main = return ()