module Script where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Binary as Binary
import Data.IORef
import System.IO.Unsafe

import Types

-- === test1 ===
sayHello :: IO ()
sayHello = putStrLn "Hello!"

printFoo :: [Foo] -> IO ()
printFoo = print

printFooBS :: BS.ByteString -> IO ()
printFooBS bytes =
  printFoo $ Binary.decode $ BC.fromStrict bytes

-- === test2 ===
globalFoo :: IORef Foo
globalFoo = unsafePerformIO $ newIORef (Foo 0)
{-# NOINLINE globalFoo #-}

incrIOFoo :: IO ()
incrIOFoo =
  modifyIORef globalFoo $ \(Foo i) -> Foo (i + 1)

readIOFoo :: IO ()
readIOFoo = readIORef globalFoo >>= print
