-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Accum
-- Copyright   :  (c) Nickolay Kudasov 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The lazy 'AccumT' monad transformer, which adds accumulation
-- capabilities (such as declarations or document patches) to a given monad.
--
-- This monad transformer provides append-only accumulation
-- during the computation. For more general access, use
-- "Control.Monad.State" instead.
-----------------------------------------------------------------------------

module Control.Monad.Accum (
    -- * MonadAccum class
    MonadAccum(..),
    looks,
    -- * The accumulation monad
    Accum,
    runAccum,
    execAccum,
    evalAccum,
    mapAccum,
    -- * The accumulation monad transformer
    AccumT,
    runAccumT,
    execAccumT,
    evalAccumT,
    mapAccumT,
    ) where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Accum (
    Accum, runAccum, execAccum, evalAccum, mapAccum,
    AccumT(..), runAccumT, execAccumT, evalAccumT, mapAccumT)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select

-- | A monad with accumulation capabilities (such as declarations or
-- document patches).
class (Monoid (AccumType m), Monad m) => MonadAccum m where
    -- | The monoid of things to accumulate.
    type AccumType m

    -- | @'look'@ is an action that fetches all the previously accumulated
    -- output.
    look :: m (AccumType m)

    -- | @'add' w@ is an action that produces the output @w@.
    add :: AccumType m -> m ()

    -- | Construct an accumulation computation from a (result, output) pair.
    accum :: (AccumType m -> (a, AccumType m)) -> m a

instance (Monoid w, Monad m) => MonadAccum (Accum.AccumT w m) where
    type AccumType (AccumT w m) = w
    look = Accum.look
    add = Accum.add
    accum = Accum.accum

instance (MonadAccum m) => MonadAccum (ContT r m) where
    type AccumType (ContT r m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

instance (MonadAccum m) => MonadAccum (ExceptT e m) where
    type AccumType (ExceptT e m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

instance (MonadAccum m) => MonadAccum (IdentityT m) where
    type AccumType (IdentityT m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

instance (MonadAccum m) => MonadAccum (MaybeT m) where
    type AccumType (MaybeT m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

instance (MonadAccum m) => MonadAccum (ReaderT r m) where
    type AccumType (ReaderT r m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

instance (MonadAccum m) => MonadAccum (SelectT r m) where
    type AccumType (SelectT r m) = AccumType m
    look = lift look
    add w = lift (add w)
    accum f = lift (accum f)

-- | @'look'@ is an action that retrieves a function of the previously
-- accumulated output.
looks :: (MonadAccum m) => (AccumType m -> a) -> m a
looks f = f <$> look
