module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.INotify (withINotify, addWatch, EventVariety(..))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Char8 as B

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: waitfordownload filename"
  exitFailure

watchForDeletion :: B.ByteString -> IO ()
watchForDeletion filename = do
  deleted <- newEmptyMVar
  withINotify $ \ino -> do
    addWatch
      ino
      [DeleteSelf] -- watch for .part file to be deleted
      filename
      (\_ -> putMVar deleted ()) -- signal with MVar that we continue
    takeMVar deleted -- wait for the putMVar from the watch
    exitSuccess

main :: IO ()
main = do
  args <- getArgs
  if null args then
    usage
  else
    let
      partFilename = B.pack $ head args ++ ".part" -- this is for Firefox
    in
      do
        watchForDeletion partFilename
