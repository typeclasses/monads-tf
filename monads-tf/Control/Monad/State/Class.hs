-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- = MonadState class
--
-- This module is inspired by the paper
-- /Functional Programming with Overloading and Higher-Order Polymorphism/,
--   Mark P Jones (<https://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--     Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Class (
    MonadState(..),
    modify,
    modify',
    gets,
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.RWS.Lazy qualified as LazyRWS (RWST, get, put, state)
import Control.Monad.Trans.RWS.Strict qualified as StrictRWS (RWST, get, put, state)
import Control.Monad.Trans.RWS.CPS qualified as CPSRWS (RWST, get, put, state)
import Control.Monad.Trans.State.Lazy qualified as Lazy (StateT, get, put, state)
import Control.Monad.Trans.State.Strict qualified as Strict (StateT, get, put, state)
import Control.Monad.Trans.Writer.Lazy qualified as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict qualified as Strict (WriterT)
import Control.Monad.Trans.Writer.CPS qualified as CPS (WriterT)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Select (SelectT)

-- ---------------------------------------------------------------------------

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class (Monad m) => MonadState m where
    -- | The type of the state.
    type StateType m

    -- | Return the state from the internals of the monad.
    get :: m (StateType m)
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: StateType m -> m ()
    put s = state (const ((), s))

    -- | Embed a simple state action into the monad.
    --
    -- @since 0.4.0.0
    state :: (StateType m -> (a, StateType m)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

    {-# MINIMAL state | get, put #-}

-- | Monadic state transformer.
--
-- Maps an old state to a new state inside a state monad.
-- The old state is thrown away.
--
-- > Main> :t modify ((+1) :: Int -> Int)
-- > modify (...) :: (StateType m ~ Int, MonadState m) => m ()
--
-- This says that @modify (+1)@ acts over any
-- Monad that is a member of the @MonadState@ class,
-- with an @Int@ state.
modify :: (MonadState m) => (StateType m -> StateType m) -> m ()
modify f = do
    s <- get
    put (f s)

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- @since 0.4.0.0
modify' :: (MonadState m) => (StateType m -> StateType m) -> m ()
modify' f = do
  s' <- get
  put $! f s'

-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: (MonadState m) => (StateType m -> a) -> m a
gets f = f <$> get

instance (Monad m) => MonadState (Lazy.StateT s m) where
    type StateType (Lazy.StateT s m) = s
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance (Monad m) => MonadState (Strict.StateT s m) where
    type StateType (Strict.StateT s m) = s
    get = Strict.get
    put = Strict.put
    state = Strict.state

instance (Monad m, Monoid w) => MonadState (LazyRWS.RWST r w s m) where
    type StateType (LazyRWS.RWST r w s m) = s
    get = LazyRWS.get
    put = LazyRWS.put
    state = LazyRWS.state

instance (Monad m, Monoid w) => MonadState (StrictRWS.RWST r w s m) where
    type StateType (StrictRWS.RWST r w s m) = s
    get = StrictRWS.get
    put = StrictRWS.put
    state = StrictRWS.state

-- | @since 0.4.0.0
instance (Monad m, Monoid w) => MonadState (CPSRWS.RWST r w s m) where
    type StateType (CPSRWS.RWST r w s m) = s
    get = CPSRWS.get
    put = CPSRWS.put
    state = CPSRWS.state

-- ---------------------------------------------------------------------------
-- Instances for other monad transformers

instance (MonadState m) => MonadState (ContT r m) where
    type StateType (ContT r m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadState m) => MonadState (ExceptT e m) where
    type StateType (ExceptT e m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadState m) => MonadState (IdentityT m) where
    type StateType (IdentityT m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadState m) => MonadState (MaybeT m) where
    type StateType (MaybeT m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadState m) => MonadState (ReaderT r m) where
    type StateType (ReaderT r m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState m) => MonadState (Lazy.WriterT w m) where
    type StateType (Lazy.WriterT w m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState m) => MonadState (Strict.WriterT w m) where
    type StateType (Strict.WriterT w m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 0.4.0.0
instance (Monoid w, MonadState m) => MonadState (CPS.WriterT w m) where
    type StateType (CPS.WriterT w m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 0.4.0.0
instance (Monoid w, MonadState m) => MonadState (AccumT w m) where
    type StateType (AccumT w m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 0.4.0.0
instance MonadState m => MonadState (SelectT r m) where
    type StateType (SelectT r m) = StateType m
    get = lift get
    put = lift . put
    state = lift . state
