{-# LANGUAGE ForeignFunctionInterface #-}

module Lib (
  someFunc,
  incrementCounter,
  counterStore,
)
where

import Control.Monad (when)
import Foreign (Ptr, Storable (peek, poke), malloc)
import Foreign.Store (Store, lookupStore, newStore, readStore)
import System.IO.Unsafe (unsafePerformIO)

someFunc :: IO ()
someFunc = return "asdf" >>= putStrLn

-- A global counter stored in a foreign store
counterStore :: IO (Store (Ptr Int), Int)
counterStore = do
  storeExists <- lookupStore 0
  case storeExists of
    Just store -> do
      ptr <- readStore store
      val <- peek ptr
      return (store, val)
    Nothing -> do
      ptr <- malloc
      poke ptr 0
      store <- newStore ptr
      return (store, 0)

-- Increment the counter
incrementCounter :: IO ()
incrementCounter = do
  (store, _val) <- counterStore
  ptr <- readStore store
  val <- peek ptr
  let newVal = val + 1
  poke ptr newVal
  putStrLn $ "Counter: " ++ show newVal

-- >>> length "Hello world"
-- 11

-- >>> unwords . words $ " hello    world "
-- "hello world"
