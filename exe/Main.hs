module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TStack

import GHC.Conc

-- | This example illustrates GHC's
-- fairness policy, and models the
-- "hungry-hungry hippos" model of
-- concurrent consumption
main :: IO ()
main = do
  tstack <- newTStack
  forkIO . forever . void $ do
    -- Will block until we get an item
    item <- atomically (pop tstack)
    showThread
    putStrLn $ "You entered: " <> item
  forkIO . forever . void $ do
    -- Will block until we get an item
    item <- atomically (pop tstack)
    showThread
    putStrLn $ "You entered: " <> item
  forever $ do
    line <- getLine
    -- Will push an item when we get it
    atomically (push line tstack)
      where
        showThread = do
          tid <- myThreadId
          putStrLn $ "Printed by Thread: " <> show tid
