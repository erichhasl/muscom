{-# LANGUAGE ForeignFunctionInterface, InterruptibleFFI #-}

module Audio where

import System.Process
import Foreign.C.String

foreign import ccall interruptible "audiolib.h play"
    c_play :: CString -> IO ()

playAudio :: FilePath -> IO ()
playAudio path = newCString path >>= c_play
