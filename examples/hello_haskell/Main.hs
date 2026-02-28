{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign
import GHC.Exts

-- These two foreign imports are mandatory for interacting with Typst,
--  and they shouldn't be modified.
foreign import ccall "wasm_minimal_protocol_write_args_to_buffer"
  wasm_minimal_protocol_write_args_to_buffer :: Addr# -> IO ()

foreign import ccall "wasm_minimal_protocol_send_result_to_host"
  wasm_minimal_protocol_send_result_to_host :: Addr# -> Int# -> IO ()

-- Wrapper functions for the two functions for interacting with Typst.
-- These are really just convenient wrappers, and you can directly interact
--  with Typst using the two foreign import functions above, if you want.
-- However you will just end up reimplementing these two functions.
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

-- Implementation of concrete plugin functions
--
-- To implement a function in the Typst plugin, you need to do three things:
--  1. Implement the Haskell function. It should take a number of `Int` parameters,
--      and return `IO Int`.
--  2. Export the Haskell function using a `foreign export ccall`.
--  3. Be explicitly exported using command line arguments when building.
-- Additionally:
--  4. `hs_init_wrapped` must be exported using command line arguments when building.
--
-- In our example, 1 and 2 are implemented in pairs in this Haskell file.
-- 3 and 4 appear in the build command in README, as in the arguments:
--   -optl-Wl,--export=hs_init_wrapped,--export=hello,--export=double_it,--export=concatenate,--export=shuffle,--export=returns_ok,--export=will_panic,--export=returns_err

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

-- The `main` function here is just a placeholder to make GHC happy.
main :: IO ()
main = return ()