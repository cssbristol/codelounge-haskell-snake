{-# LANGUAGE CPP #-}

{- |
This package exists as a workaround for GHC bug #2189, \"@hSetBuffering stdin
NoBuffering@ doesn't work on Windows\". It provides functionality for reading
from standard input without buffering, in a way that works under GHC on
Windows as well as other configurations. This is useful for key-driven console
applications such as roguelikes.
-}
module System.IO.NoBufferingWorkaround (
    initGetCharNoBuffering, getCharNoBuffering
) where

#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)

-- See http://stackoverflow.com/a/13370293/925960

import Data.Char(chr)
import Foreign.C.Types(CInt(..))

foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt

initGetCharNoBuffering = return ()
getCharNoBuffering = fmap (chr . fromEnum) c_getch

#else

import System.IO(hSetBuffering, BufferMode(NoBuffering), stdin)

initGetCharNoBuffering = hSetBuffering stdin NoBuffering
getCharNoBuffering = getChar

#endif

-- | Must be called before invoking 'getCharNoBuffering'.
initGetCharNoBuffering :: IO ()
-- | Behaves like 'getChar', but never does any buffering.
getCharNoBuffering :: IO Char
