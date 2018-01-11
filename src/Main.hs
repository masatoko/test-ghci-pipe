{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM, forever)
import           Criterion.Main
import qualified Data.Binary                as Binary

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified GHCi
import           Types

main :: IO ()
main = do
  test1
  test2

-- | Send Binary data to ghci via pipe
test1 :: IO ()
test1 = do
  ghci <- GHCi.genGhciPipe "Script"
  GHCi.eval ghci ":set -XOverloadedStrings"
  --
  GHCi.evalBS ghci "sayHello"
  GHCi.readResult ghci >>= print
  --
  let calc f = do
        f $ map Foo [0..1000]
        res <- GHCi.readResult ghci
        case res of
          GHCi.Result fs ->
            return . sum . map unFoo $ read fs
          _ -> return 0

  let strGhci = calc $ \foos -> GHCi.eval ghci $ "printFoo $ " ++ show foos
  let strGhciBS = calc $ \foos -> GHCi.evalBS ghci $ BC.toStrict $ BC.pack $ "printFoo $ " ++ show foos
  let byteGhci = calc $ \foos -> do
                  let script = "printFooBS " ++ show (Binary.encode foos)
                  GHCi.eval ghci script
  let byteGhciBS = calc $ \foos -> do
                    let script = "printFooBS " ++ show (Binary.encode foos)
                    GHCi.evalBS ghci $ BC.toStrict $ BC.pack script
  defaultMain
    [ bench "String" (whnfIO strGhci)
    , bench "StringBS" (whnfIO strGhciBS)
    , bench "Binary" (whnfIO byteGhci)
    , bench "BinaryBS" (whnfIO byteGhciBS)
    ]
    
-- | Simulate global variable in ghci
test2 :: IO ()
test2 = do
  ghci <- GHCi.genGhciPipe "Script"
  let eval = GHCi.eval ghci
      printRes = GHCi.readResult ghci >>= print

  eval "readIOFoo"
  printRes

  forever $ do
    eval "incrIOFoo"
    eval "readIOFoo"
    printRes
    threadDelay 1000000
