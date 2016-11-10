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

foreign import ccall unsafe "conio.h kbhit"
    c_kbhit :: IO CInt

initGetCharNoBuffering = return ()
getCharNoBuffering = do k <- c_kbhit
                        if k == 0 then
                          return Nothing
                        else
                          fmap (Just . chr . fromEnum) c_getch

#else

import System.IO(hSetBuffering, BufferMode(NoBuffering), hReady, stdin)

initGetCharNoBuffering = hSetBuffering stdin NoBuffering
getCharNoBuffering = do b <- hReady stdin
                        if not b then
                          return Nothing
                        else
                          fmap Just getChar

#endif

-- | Must be called before invoking 'getCharNoBuffering'.
initGetCharNoBuffering :: IO ()
-- | Behaves like 'getChar', but never does any buffering.
getCharNoBuffering :: IO (Maybe Char)
