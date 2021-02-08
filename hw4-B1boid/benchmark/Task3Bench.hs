module Task3Bench
  ( hashtableBenchs
  ) where

import Criterion.Main (Benchmark, defaultMain, bgroup, bench, nfIO)
import Control.Concurrent.Async (async, wait)
import Task3 (ConcurrentHashTable (..), newCHT, getCHT, putCHT)

-- | Benchs for hashtable
hashtableBenchs :: IO ()
hashtableBenchs = defaultMain [ bgroup "hashtable" [ singleThreadBench, concurrentBench ]]

-- | Bench single thread test 10^4 operations
singleThreadBench :: Benchmark
singleThreadBench = bgroup "Single Thread"
  [ bench "10^4 PUT operations (10^5 ~ 40 s)" $ nfIO $ putTest,
    bench "10^4 PUT and 10^4 GET operations" $ nfIO $ putAndGetTest]
  where
    dataTest = map (\a -> (a, a)) [0 .. 10000]
    putTest = do
      hashtable <- newCHT :: IO (ConcurrentHashTable Int Int)
      mapM_ (\(k, v) -> putCHT k v hashtable) dataTest
    putAndGetTest = do
      hashtable <- newCHT :: IO (ConcurrentHashTable Int Int)
      mapM_ (\(k, v) -> putCHT k v hashtable) dataTest
      mapM_ (\(k, _) -> getCHT k hashtable) dataTest

-- | Concurrent test 10^5 operations
concurrentBench :: Benchmark
concurrentBench = bgroup "Concurrent"
  [ bench "(20 threads): 10^5 PUT operations" $ nfIO $ putTest,
    bench "(40 threads): 10^5 PUT and GET operations" $ nfIO $ putAndGetTest]
  where
    putTest = do
      hashtable <- newCHT :: IO (ConcurrentHashTable Int Int)
      threads <- mapM (\_ -> async $ mapM_ (\a -> putCHT a a hashtable) [0 .. 5000])
                      [1 .. 20 :: Int]
      mapM_ wait threads
    putAndGetTest = do
      hashtable <- newCHT :: IO (ConcurrentHashTable Int Int)
      threadsPut <- mapM (\_ -> async $ mapM_ (\a -> putCHT a a hashtable) [0 .. 5000])
                         [1 .. 20 :: Int]
      threadsGet <- mapM (\_ -> async $ mapM_ (\a -> getCHT a hashtable) [0 .. 5000])
                         [1 .. 20 :: Int]
      mapM_ wait threadsPut
      mapM_ wait threadsGet
