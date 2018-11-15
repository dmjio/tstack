module Control.Concurrent.STM.TStack
 ( TStack
 , push
 , pop
 , tryPop
 , newTStack
 , size
 , peek
 , tryPeek
 , isEmpty
 ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

-- | Internal stack type is just a linked-list.
type Stack a = [a]

-- | Push O(1)
pushStack :: a -> Stack a -> Stack a
pushStack x xs = x : xs

-- | Pop O(1)
popStack :: Stack a -> (Maybe a, Stack a)
popStack [] = (Nothing, [])
popStack (x:xs) = (Just x, xs)

-- | TStack, transactional Stack
type TStack a = TVar (Stack a)

-- | Creates new `TStack`
newTStack :: IO (TStack a)
newTStack = newTVarIO mempty

-- | `push` O(1)
push :: a -> TStack a -> STM ()
push x svar = modifyTVar' svar (x:)

-- | `size` O(n)
size :: TStack a -> STM Int
size tstack = length <$> readTVar tstack

-- | `peek`, blocks until element added
peek :: TStack a -> STM a
peek tstack = do
  stack <- readTVar tstack
  case popStack stack of
    (Nothing, xs) -> retry
    (Just x, xs) -> pure x

-- | `isEmpty`, blocks
isEmpty :: TStack a -> STM Bool
isEmpty tstack = do
  stack <- readTVar tstack
  pure $ case popStack stack of
    (Nothing, []) -> True
    (_, _) -> False

-- | `tryPeek`, non-blocking
tryPeek :: TStack a -> STM (Maybe a)
tryPeek tstack =
  fst . popStack
    <$> readTVar tstack

-- | `pop` O(1)
-- Will block until value is read off the stack
pop :: TStack a -> STM a
pop tstack = do
  stack <- readTVar tstack
  case popStack stack of
    (Nothing, _) ->
      retry
    (Just x, xs) ->
      x <$ writeTVar tstack xs

-- | `pop` O(1)
-- Will block until value is read off the stack
tryPop :: TStack a -> STM (Maybe a)
tryPop tstack =
  fst . popStack
    <$> readTVar tstack
