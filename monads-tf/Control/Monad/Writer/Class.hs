-- |
-- Module      :  Control.Monad.Writer.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Chris Martin <chris@typeclasses.com>
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
module Control.Monad.Writer.Class
  ( MonadWriter (..),
    listens,
    censor,
  )
where

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Accum qualified as Accum
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Identity qualified as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Maybe qualified as Maybe
import Control.Monad.Trans.RWS.CPS qualified as CPSRWS
import Control.Monad.Trans.RWS.Lazy qualified as LazyRWS
import Control.Monad.Trans.RWS.Strict qualified as StrictRWS
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Control.Monad.Trans.State.Strict qualified as Strict
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict

-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement).
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid (WriterType m), Monad m) => MonadWriter m where
  {-# MINIMAL (writer | tell), listen, pass #-}

  -- | The type of the writer's output.
  type WriterType m

  -- | @'writer' (a,w)@ embeds a simple writer action.
  writer :: (a, WriterType m) -> m a
  writer ~(a, w) = do
    tell w
    return a

  -- | @'tell' w@ is an action that produces the output @w@.
  tell :: WriterType m -> m ()
  tell w = writer ((), w)

  -- | @'listen' m@ is an action that executes the action @m@ and adds
  -- its output to the value of the computation.
  listen :: m a -> m (a, WriterType m)

  -- | @'pass' m@ is an action that executes the action @m@, which
  -- returns a value and a function, and returns the value, applying
  -- the function to the output.
  pass :: m (a, WriterType m -> WriterType m) -> m a

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
listens :: (MonadWriter m) => (WriterType m -> b) -> m a -> m (a, b)
listens f m = do
  ~(a, w) <- listen m
  return (a, f w)

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: (MonadWriter m) => (WriterType m -> WriterType m) -> m a -> m a
censor f m = pass $ do
  a <- m
  return (a, f)

-- | @since 0.4.0.0
instance (Monoid w) => MonadWriter ((,) w) where
  type WriterType ((,) w) = w
  writer ~(a, w) = (w, a)
  tell w = (w, ())
  listen ~(w, a) = (w, (a, w))
  pass ~(w, (a, f)) = (f w, a)

-- | @since 0.4.0.0
instance (Monoid w, Monad m) => MonadWriter (CPS.WriterT w m) where
  type WriterType (CPS.WriterT w m) = w
  writer = CPS.writer
  tell = CPS.tell
  listen = CPS.listen
  pass = CPS.pass

instance (Monoid w, Monad m) => MonadWriter (Lazy.WriterT w m) where
  type WriterType (Lazy.WriterT w m) = w
  writer = Lazy.writer
  tell = Lazy.tell
  listen = Lazy.listen
  pass = Lazy.pass

instance (Monoid w, Monad m) => MonadWriter (Strict.WriterT w m) where
  type WriterType (Strict.WriterT w m) = w
  writer = Strict.writer
  tell = Strict.tell
  listen = Strict.listen
  pass = Strict.pass

-- | @since 0.4.0.0
instance (Monoid w, Monad m) => MonadWriter (CPSRWS.RWST r w s m) where
  type WriterType (CPSRWS.RWST r w s m) = w
  writer = CPSRWS.writer
  tell = CPSRWS.tell
  listen = CPSRWS.listen
  pass = CPSRWS.pass

instance (Monoid w, Monad m) => MonadWriter (LazyRWS.RWST r w s m) where
  type WriterType (LazyRWS.RWST r w s m) = w
  writer = LazyRWS.writer
  tell = LazyRWS.tell
  listen = LazyRWS.listen
  pass = LazyRWS.pass

instance (Monoid w, Monad m) => MonadWriter (StrictRWS.RWST r w s m) where
  type WriterType (StrictRWS.RWST r w s m) = w
  writer = StrictRWS.writer
  tell = StrictRWS.tell
  listen = StrictRWS.listen
  pass = StrictRWS.pass

-- ---------------------------------------------------------------------------
-- Instances for other transformers

-- | @since 0.4.0.0
instance (MonadWriter m) => MonadWriter (ExceptT e m) where
  type WriterType (ExceptT e m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Except.liftListen listen
  pass = Except.liftPass pass

instance (MonadWriter m) => MonadWriter (IdentityT m) where
  type WriterType (IdentityT m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Identity.mapIdentityT listen
  pass = Identity.mapIdentityT pass

instance (MonadWriter m) => MonadWriter (MaybeT m) where
  type WriterType (MaybeT m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Maybe.liftListen listen
  pass = Maybe.liftPass pass

instance (MonadWriter m) => MonadWriter (ReaderT r m) where
  type WriterType (ReaderT r m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = mapReaderT listen
  pass = mapReaderT pass

instance (MonadWriter m) => MonadWriter (Lazy.StateT s m) where
  type WriterType (Lazy.StateT s m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Lazy.liftListen listen
  pass = Lazy.liftPass pass

instance (MonadWriter m) => MonadWriter (Strict.StateT s m) where
  type WriterType (Strict.StateT s m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Strict.liftListen listen
  pass = Strict.liftPass pass

-- | There are two valid instances for 'AccumT'. It could either:
--
--   1. Lift the operations to the inner @MonadWriter@
--   2. Handle the operations itself, à la a @WriterT@.
--
--   This instance chooses (1), reflecting that the intent
--   of 'AccumT' as a type is different than that of @WriterT@.
--
-- @since 0.4.0.0
instance
  ( Monoid w,
    MonadWriter m
  ) =>
  MonadWriter (AccumT w m)
  where
  type WriterType (AccumT w m) = WriterType m
  writer = lift . writer
  tell = lift . tell
  listen = Accum.liftListen listen
  pass = Accum.liftPass pass
