-- | Module: Control.Monad.Accum
-- Copyright: (C) Koz Ross 2022, Manuel Bärenz 2021
-- License: BSD-3-Clause (see the LICENSE file)
-- Maintainer: Chris Martin <chris@typeclasses.com>
-- Stability: Experimental
-- Portability: non-portable (type families)
--
-- [Computation type:] Accumulation (either append-only state, or writer with
-- the ability to read all previous input).
--
-- [Binding strategy:] Binding a function to a monadic value monoidally
-- accumulates the subcomputations (that is, using '<>').
--
-- [Useful for:] Logging, patch-style tracking.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Control.Monad.Trans.Accum.Accum' w a@
--
-- = A note on commutativity
--
-- Some effects are /commutative/: it doesn't matter which you resolve first, as
-- all possible orderings of commutative effects are isomorphic. Consider, for
-- example, the reader and state effects, as exemplified by 'ReaderT' and
-- 'StrictState.StateT' respectively. If we have
-- @'ReaderT' r ('StrictState.State' s) a@, this is
-- effectively @r -> 'StrictState.State' s a ~ r -> s -> (a, s)@; if we instead have
-- @'StrictState.StateT' s ('Control.Monad.Trans.Reader.Reader' r) a@, this is effectively
-- @s -> 'Control.Monad.Trans.Reader' r (a, s) ~ s -> r -> (a, s)@. Since we
-- can always reorder function arguments (for example, using 'flip', as in
-- this case) without changing the result, these are
-- isomorphic, showing that reader and state are /commutative/, or, more
-- precisely, /commute with each other/.
--
-- However, this isn't generally the case. Consider instead the error and state
-- effects, as exemplified by 'MaybeT' and 'StrictState.StateT' respectively.
-- If we have @'MaybeT' ('Control.Monad.Trans.State.Strict.State' s) a@, this
-- is effectively @'State' s ('Maybe' a) ~ s -> ('Maybe' a, s)@: put simply,
-- the error can occur only in the /result/, but
-- not the state, which always \'survives\'. On the other hand, if we have
-- @'StrictState.StateT' s 'Maybe' a@, this is instead @s -> 'Maybe' (a, s)@: here,
-- if we error, we lose /both/ the state and the result! Thus, error and state effects
-- do /not/ commute with each other.
--
-- As the monads-tf is capability-based, we support any ordering of non-commutative
-- effects on an equal footing. Indeed, if you wish to use
-- 'Control.Monad.State.Class.MonadState', for
-- example, whether your final monadic stack ends up being @'MaybeT'
-- ('Control.Monad.Trans.State.Strict.State' s)
-- a@, @'StrictState.StateT' s 'Maybe' a@, or anything else, you will be able to write your
-- desired code without having to consider such differences. However, the way we
-- /implement/ these capabilities for any given transformer (or rather, any
-- given transformed stack) /is/ affected by this ordering unless the effects in
-- question are commutative.
--
-- We note in this module which effects the accumulation effect does and doesn't
-- commute with; we also note on implementations with non-commutative
-- transformers what the outcome will be. Note that, depending on how the
-- \'inner monad\' is structured, this may be more complex than we note: we
-- describe only what impact the \'outer effect\' has, not what else might be in
-- the stack.
--
-- = Commutativity of accumulation
--
-- The accumulation effect commutes with the identity effect ('IdentityT'),
-- reader, writer or state effects ('ReaderT', 'StrictWriter.WriterT', 'StrictState.StateT' and any
-- combination, including 'StrictRWS.RWST' for example) and with itself. It does /not/
-- commute with anything else.
module Control.Monad.Accum
  ( -- * Type class
    MonadAccum (..),

    -- * Other functions
    looks,
  )
where

import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)

-- | The capability to accumulate. This can be seen in one of two ways:
--
-- * A 'Control.Monad.State.Class.MonadState' which can only append (using '<>'); or
-- * A 'Control.Monad.Writer.Class.MonadWriter' (limited to
-- 'Control.Monad.Writer.Class.tell') with the ability to view the result of all previous
-- 'Control.Monad.Writer.Class.tell's.
--
-- = Laws
--
-- 'accum' should obey the following:
--
-- 1. @'accum' ('const' (x, 'mempty'))@ @=@ @'pure' x@
-- 2. @'accum' f '*>' 'accum' g@ @=@
-- @'accum' '$' \acc -> let (_, v) = f acc
--                          (res, w) = g (acc '<>' v) in (res, v '<>' w)@
--
-- If you choose to define 'look' and 'add' instead, their definitions must obey
-- the following:
--
-- 1. @'look' '*>' 'look'@ @=@ @'look'@
-- 2. @'add' 'mempty'@ @=@ @'pure' ()@
-- 3. @'add' x '*>' 'add' y@ @=@ @'add' (x '<>' y)@
-- 4. @'add' x '*>' 'look'@ @=@ @'look' '>>=' \w -> 'add' x '$>' w '<>' x@
--
-- If you want to define both, the relationship between them is as follows.
-- These are also the default definitions.
--
-- 1. @'look'@ @=@ @'accum' '$' \acc -> (acc, mempty)@
-- 2. @'add' x@ @=@ @'accum' '$' \acc -> ('()', x)@
-- 3. @'accum' f@ @=@ @'look' >>= \acc -> let (res, v) = f acc in 'add' v '$>' res@
--
-- @since 0.4.0.0
class (Monoid (AccumType m), Monad m) => MonadAccum m where
  -- | The type of the accumulator.
  type AccumType m

  -- | Retrieve the accumulated result so far.
  look :: m (AccumType m)
  look = accum (,mempty)

  -- | Append a value to the result.
  add :: (AccumType m) -> m ()
  add x = accum $ const ((), x)

  -- | Embed a simple accumulation action into the monad.
  accum :: ((AccumType m) -> (a, (AccumType m))) -> m a
  accum f = look >>= \acc -> let (res, v) = f acc in add v $> res

  {-# MINIMAL accum | look, add #-}

-- | @since 0.4.0.0
instance (Monoid w) => MonadAccum (AccumT w Identity) where
  type AccumType (AccumT w Identity) = w
  look = Accum.look
  add = Accum.add
  accum = Accum.accum

-- | The accumulated value \'survives\' an error: even if the
-- computation fails to deliver a result, we still have an accumulated value.
--
-- @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (MaybeT m) where
  type AccumType (MaybeT m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | The continuation can see, and interact with, the accumulated value.
--
-- @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (ContT r m) where
  type AccumType (ContT r m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | The accumulated value \'survives\' an exception: even if the computation
-- fails to deliver a result, we still have an accumulated value.
--
-- @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (ExceptT e m) where
  type AccumType (ExceptT e m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (IdentityT m) where
  type AccumType (IdentityT m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (CPSRWS.RWST r w s m) where
  type AccumType (CPSRWS.RWST r w s m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m, Monoid w) => MonadAccum (LazyRWS.RWST r w s m) where
  type AccumType (LazyRWS.RWST r w s m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m, Monoid w) => MonadAccum (StrictRWS.RWST r w s m) where
  type AccumType (StrictRWS.RWST r w s m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (ReaderT r m) where
  type AccumType (ReaderT r m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | The \'ranking\' function gains the ability to accumulate @w@s each time it
-- is called. The final result will include the entire log of all such calls.
--
-- @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (SelectT r m) where
  type AccumType (SelectT r m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (LazyState.StateT s m) where
  type AccumType (LazyState.StateT s m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (StrictState.StateT s m) where
  type AccumType (StrictState.StateT s m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m) => MonadAccum (CPSWriter.WriterT w m) where
  type AccumType (CPSWriter.WriterT w m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum


-- | @since 0.4.0.0
instance (MonadAccum m, Monoid w) => MonadAccum (LazyWriter.WriterT w m) where
  type AccumType (LazyWriter.WriterT w m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | @since 0.4.0.0
instance (MonadAccum m, Monoid w) => MonadAccum (StrictWriter.WriterT w m) where
  type AccumType (StrictWriter.WriterT w m) = AccumType m
  look = lift look
  add = lift . add
  accum = lift . accum

-- | Retrieve a function of the accumulated value.
--
-- @since 0.4.0.0
looks ::
  forall (a :: Type) (m :: Type -> Type).
  (MonadAccum m) =>
  (AccumType m -> a) ->
  m a
looks f = f <$> look
