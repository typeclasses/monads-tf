-- |
-- Module      :  Control.Monad.Reader.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001,
--                (c) Jeff Newbern 2003-2007,
--                (c) Andriy Palamarchuk 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Chris Martin <chris@typeclasses.com>
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- [Computation type:] Computations which read values from a shared environment.
--
-- [Binding strategy:] Monad values are functions from the environment to a value.
-- The bound function is applied to the bound value, and both have access
-- to the shared environment.
--
-- [Useful for:] Maintaining variable bindings, or other shared environment.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Reader' [(String,Value)] a@
--
-- The 'Reader' monad (also called the Environment monad).
-- Represents a computation, which can read values from
-- a shared environment, pass values from function to function,
-- and execute sub-computations in a modified environment.
-- Using 'Reader' monad for such computations is often clearer and easier
-- than using the 'Control.Monad.State.State' monad.
--
--   Inspired by the paper
--   /Functional Programming with Overloading and Higher-Order Polymorphism/,
--     Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--     Advanced School of Functional Programming, 1995.
module Control.Monad.Reader.Class
  ( MonadReader (..),
    asks,
  )
where

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Accum qualified as Accum
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Cont qualified as Cont
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.RWS.CPS qualified as CPSRWS
import Control.Monad.Trans.RWS.Lazy qualified as LazyRWS
import Control.Monad.Trans.RWS.Strict qualified as StrictRWS
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as ReaderT
import Control.Monad.Trans.Select (SelectT (SelectT), runSelectT)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Control.Monad.Trans.State.Strict qualified as Strict
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

-- | See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
class (Monad m) => MonadReader m where
  {-# MINIMAL (ask | reader), local #-}

  -- | The type of the environment.
  type EnvType m

  -- | Retrieves the monad environment.
  ask :: m (EnvType m)
  ask = reader id

  -- | Executes a computation in a modified environment.
  local ::
    -- | The function to modify the environment.
    (EnvType m -> EnvType m) ->
    -- | @Reader@ to run in the modified environment.
    m a ->
    m a

  -- | Retrieves a function of the current environment.
  reader ::
    -- | The selector function to apply to the environment.
    (EnvType m -> a) ->
    m a
  reader f = f <$> ask

-- | Retrieves a function of the current environment.
asks ::
  (MonadReader m) =>
  -- | The selector function to apply to the environment.
  (EnvType m -> a) ->
  m a
asks = reader

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader ((->) r) where
  type EnvType ((->) r) = r
  ask = id
  local f m = m . f

instance (Monad m) => MonadReader (ReaderT r m) where
  type EnvType (ReaderT r m) = r
  ask = ReaderT.ask
  local = ReaderT.local
  reader = ReaderT.reader

-- | @since 0.4.0.0
instance (Monad m, Monoid w) => MonadReader (CPSRWS.RWST r w s m) where
  type EnvType (CPSRWS.RWST r w s m) = r
  ask = CPSRWS.ask
  local = CPSRWS.local
  reader = CPSRWS.reader

instance (Monoid w, Monad m) => MonadReader (LazyRWS.RWST r w s m) where
  type EnvType (LazyRWS.RWST r w s m) = r
  ask = LazyRWS.ask
  local = LazyRWS.local
  reader = LazyRWS.reader

instance (Monoid w, Monad m) => MonadReader (StrictRWS.RWST r w s m) where
  type EnvType (StrictRWS.RWST r w s m) = r
  ask = StrictRWS.ask
  local = StrictRWS.local
  reader = StrictRWS.reader

-- ---------------------------------------------------------------------------
-- Instances for other transformers

instance (MonadReader m) => MonadReader (ContT r m) where
  type EnvType (ContT r m) = EnvType m
  ask = lift ask
  local = Cont.liftLocal ask local
  reader = lift . reader

instance (MonadReader m) => MonadReader (ExceptT e m) where
  type EnvType (ExceptT e m) = EnvType m
  ask = lift ask
  local = mapExceptT . local
  reader = lift . reader

instance (MonadReader m) => MonadReader (IdentityT m) where
  type EnvType (IdentityT m) = EnvType m
  ask = lift ask
  local = mapIdentityT . local
  reader = lift . reader

instance (MonadReader m) => MonadReader (MaybeT m) where
  type EnvType (MaybeT m) = EnvType m
  ask = lift ask
  local = mapMaybeT . local
  reader = lift . reader

instance (MonadReader m) => MonadReader (Lazy.StateT s m) where
  type EnvType (Lazy.StateT s m) = EnvType m
  ask = lift ask
  local = Lazy.mapStateT . local
  reader = lift . reader

instance (MonadReader m) => MonadReader (Strict.StateT s m) where
  type EnvType (Strict.StateT s m) = EnvType m
  ask = lift ask
  local = Strict.mapStateT . local
  reader = lift . reader

-- | @since 0.4.0.0
instance (Monoid w, MonadReader m) => MonadReader (CPS.WriterT w m) where
  type EnvType (CPS.WriterT w m) = EnvType m
  ask = lift ask
  local = CPS.mapWriterT . local
  reader = lift . reader

instance (Monoid w, MonadReader m) => MonadReader (Lazy.WriterT w m) where
  type EnvType (Lazy.WriterT w m) = EnvType m
  ask = lift ask
  local = Lazy.mapWriterT . local
  reader = lift . reader

instance (Monoid w, MonadReader m) => MonadReader (Strict.WriterT w m) where
  type EnvType (Strict.WriterT w m) = EnvType m
  ask = lift ask
  local = Strict.mapWriterT . local
  reader = lift . reader

-- | @since 0.4.0.0
instance
  ( Monoid w,
    MonadReader m
  ) =>
  MonadReader (AccumT w m)
  where
  type EnvType (AccumT w m) = EnvType m
  ask = lift ask
  local = Accum.mapAccumT . local
  reader = lift . reader

-- | @since 0.4.0.0
instance
  ( MonadReader m
  ) =>
  MonadReader (SelectT r m)
  where
  type EnvType (SelectT r m) = EnvType m
  ask = lift ask

  -- there is no Select.liftLocal
  local f m = SelectT $ \c -> do
    r <- ask
    local f (runSelectT m (local (const r) . c))
  reader = lift . reader
