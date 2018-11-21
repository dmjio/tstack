# tstack
A concurrent, thread-safe, transactional Stack

### Build
```
nix-build
```

### Develop
```
$ nix-shell --command 'runghc Setup.hs build'
```

### Example
```haskell
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
```

### Result
```bash
$ nix-build && ./result/bin/example +RTS -N
a
Printed by Thread: ThreadId 2
You entered: a
b
Printed by Thread: ThreadId 3
You entered: b
c
Printed by Thread: ThreadId 2
You entered: c
d
Printed by Thread: ThreadId 3
You entered: d
e
Printed by Thread: ThreadId 2
You entered: e
```
