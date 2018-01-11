module GHCi where

import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever, unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List             (isPrefixOf)
import           Data.Maybe            (isJust)
import           Safe                  (readMay)
import           System.IO
import           System.Process

data GhciPipe = GhciPipe
  { ghciIn  :: Handle
  , ghciOut :: Handle
  }

data Message
  = Output String
  | ParseFailure String
  | Result String
  deriving (Eq, Show)

genGhciPipe :: String -> IO GhciPipe
genGhciPipe moduleName = do
  (Just hin, Just hout, Just herr, _ph) <- createProcess (proc "ghc/bin/ghci-8.0.1.exe" [])
    { std_out = CreatePipe
    , std_err = CreatePipe
    , std_in = CreatePipe
    , cwd = Just "src"
    }

  -- StdErr
  forkIO $
    forever $ do
      line <- hGetLine herr
      putStrLn $ "ERROR: " ++ line

  -- Initialize
  hPutStrLn hin $ ":l " ++ moduleName
  hFlush hin
  readTillLoaded hout

  return $ GhciPipe hin hout
  where
    readTillLoaded hout = go
      where
      go = do
        line <- hGetLine hout
        putStrLn line
        unless ("Ok, modules loaded" `isPrefixOf` line) go

readResult :: GhciPipe -> IO Message
readResult ghci = evaluate <$> BC.hGetLine (ghciOut ghci)
  where
    evaluate :: BC.ByteString -> Message
    evaluate bs0 =
      case evalPart bs0 of
        Nothing  -> Output $ BC.unpack bs0
        Just bs' -> Result $ BC.unpack bs'
      where
        evalPart bs
          | BC.null bs' = Nothing
          | otherwise   =
              if isJust $ BC.find (== '>') bs'
                then evalPart bs'
                else Just bs'
          where
            bs' = BC.tail $ BC.dropWhile (/= '>') bs

--

eval :: GhciPipe -> String -> IO ()
eval ghci command = do
  hPutStrLn (ghciIn ghci) command
  hFlush (ghciIn ghci)

evalBS :: GhciPipe -> BS.ByteString -> IO ()
evalBS ghci command = do
  BC.hPutStrLn (ghciIn ghci) command
  hFlush (ghciIn ghci)
