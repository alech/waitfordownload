module Main where

import System.INotify
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay, myThreadId, killThread)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as B

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: waitfordownload filename"
  exitFailure

watchForDeletion :: B.ByteString -> IO ()
watchForDeletion filename = do
  mti <- myThreadId
  withINotify $ \ino -> do
    addWatch
      ino
      [DeleteSelf] -- watch for .part file to be deleted
      filename
      (\_ -> killThread mti) -- then just kill our thread
    threadDelay maxBound

main :: IO ()
main = do
  args <- getArgs
  if null args then
    usage
  else do
    let partFilename = B.pack $ head args ++ ".part" -- this is for Firefox
    watchForDeletion partFilename
