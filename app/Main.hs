module Main (main) where

import Lib
import System.IO
import Control.Concurrent

help :: IO ()
help = putStrLn "You need to enter base (b), modulus (m) and remainder (d) of x = d * log b,m (simpler: d = b^x (mod m))"

printSolution :: Int -> Int -> Int -> Int -> Int -> IO String
printSolution from to base modulus remainder_ = do
  -- Get id and convert to string
  tid <- myThreadId
  let solved = batchSolve [from..to] base modulus remainder_
  let result = "Result " ++ show tid ++ " : " ++ show solved
  return $! result

threadSolve :: MVar () -> Chan () -> Int -> Int -> Int -> Int -> Int -> IO()
threadSolve mutex endFlags from to base modulus remainder_ = do
  -- Compute solution (finished before getting mutex)
  solution <- printSolution from to base modulus remainder_ 
  -- Get mutex (acquires lock for output)
  takeMVar mutex
  -- Write it
  putStrLn solution
  -- Release mutex (give up lock, another thread can take over)
  putMVar mutex ()
  -- Signal end of thread
  writeChan endFlags ()

main :: IO()
main = do
  putStrLn "To enter custome values, use ghci!"
  putStrLn "You will need to enter base (b), modulus (m) and remainder (d) of x = d * log b,m (simpler: d = b^x (mod m))"
  putStrLn "Example with: 8 = 2^x (mod 13)"
  -- Disable buffering on stdout
  hSetBuffering stdout NoBuffering
  -- Number of threads to spawn
  let n = 10
  -- Init mutex and FIFO for end flags
  mutex <- newEmptyMVar
  endFlags <- newChan
  -- Spawn threads (threads are waiting for mutex before printing)
  forkIO $ threadSolve mutex endFlags 1 15 2 60 4
  forkIO $ threadSolve mutex endFlags 15 30 2 60 4
  forkIO $ threadSolve mutex endFlags 30 45 2 60 4
  forkIO $ threadSolve mutex endFlags 45 60 2 60 4
  -- Give mutex its value (threads start aquiring mutex here)
  putMVar mutex ()
  -- Read n end flags (blocks until all threads have sent their end signal)
  mapM_ (const $ readChan endFlags) [1..n] 
