{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{- |
Module      :  Control.Monad.Fibre
Copyright   :  (c) Anupam Jain 2011
License     :  GNU GPL Version 3 (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package defines Monadic functions which provide Choice and Parallelism - (&lt;||&rt;) and (&lt;&&&rt;) - that work on Monads that provide a (MonadBi m IO) instance.

Depends on the @monadbi@ library for extracting the IO actions from m. Also provides a good example of how to use the library.
-}

module Control.Monad.Fibre (
  module Control.Monad.Bi,
  (<||>), (<&&>),
) where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, putMVar)

import Control.Monad.Bi (MonadBi(..))


--------------------------
-- FUNCTION DEFINITIONS --
--------------------------

-- Choice
(<||>) :: (Monad m, MonadBi m IO) => m o -> m o -> m o
t1 <||> t2 = do
  t1io <- lower t1
  t2io <- lower t2
  x <- raise newEmptyMVar
  raise $ do
    forkIO $ t1io >>= putMVar x
    forkIO $ t2io >>= putMVar x
  raise $ takeMVar x

-- Parallelism
(<&&>) :: (Monad m, MonadBi m IO) => m o1 -> m o2 -> m (o1,o2)
t1 <&&> t2 = do
  t1io <- lower t1
  t2io <- lower t2
  x1 <- raise newEmptyMVar
  x2 <- raise newEmptyMVar
  raise $ do
    forkIO $ t1io >>= putMVar x1
    forkIO $ t2io >>= putMVar x2
  xv1 <- raise $ takeMVar x1
  xv2 <- raise $ takeMVar x2
  return (xv1,xv2)
