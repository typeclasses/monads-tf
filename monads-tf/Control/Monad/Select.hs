-- | Module: Control.Monad.Select
-- Copyright: (C) Koz Ross 2022
-- License: BSD-3-Clause (see the LICENSE file)
-- Maintainer: Chris Martin <chris@typeclasses.com>
-- Stability: Experimental
-- Portability: GHC only
--
-- [Computation type:] Backtracking search, with @r@ as a \'ranking\' or
-- \'evaluation\' type.
--
-- [Binding strategy:] Binding a function to a monadic value \'chains together\'
-- strategies; having seen the result of one search, decide which policy to use
-- to continue.
--
-- [Useful for:] Search problems.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Control.Monad.Trans.Select.Select' r a@
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
-- = Commutativity of selection
--
-- The selection effect commutes with the identity effect ('IdentityT'), but
-- nothing else.
module Control.Monad.Select
  ( -- * Type class
    MonadSelect (..),
  )
where

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.RWS.CPS qualified as CPSRWS
import Control.Monad.Trans.RWS.Lazy qualified as LazyRWS
import Control.Monad.Trans.RWS.Strict qualified as StrictRWS
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.Select qualified as Select
import Control.Monad.Trans.State.Lazy qualified as LazyState
import Control.Monad.Trans.State.Strict qualified as StrictState
import Control.Monad.Trans.Writer.CPS qualified as CPSWriter
import Control.Monad.Trans.Writer.Lazy qualified as LazyWriter
import Control.Monad.Trans.Writer.Strict qualified as StrictWriter
import Data.Functor.Identity (Identity)

-- | The capability to search with backtracking. Essentially describes a
-- \'policy function\': given the state of the search (and a \'ranking\' or
-- \'evaluation\' of each possible result so far), pick the result that's
-- currently best.
--
-- = Laws
--
-- Any instance of 'MonadSelect must follow these laws:
--
-- * @'select' ('const' x)@ @=@ @'pure' x@
-- * @'select' f '*>' 'select' g@ @=@ @'select' g@
--
-- @since 0.4.0.0
class (Monad m) => MonadSelect m where
  type SelectType m
  select :: ((a -> SelectType m) -> a) -> m a

-- | @since 0.4.0.0
instance MonadSelect (SelectT r Identity) where
  type SelectType (SelectT r Identity) = r
  select = Select.select

-- | \'Extends\' the possibilities considered by @m@ to include 'Nothing'; this
-- means that 'Nothing' gains a \'rank\' (namely, a value of @r@), and the
-- potential result could also be 'Nothing'.
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (MaybeT m) where
  type SelectType (MaybeT m) = SelectType m
  select = lift . select

-- | The continuation describes a way of choosing a \'search\' or \'ranking\'
-- strategy for @r@, based on a \'ranking\' using @r'@, given any @a@. We then
-- get a \'search\' strategy for @r@.
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (ContT r m) where
  type SelectType (ContT r m) = SelectType m
  select = lift . select

-- | \'Extends\' the possibilities considered by @m@ to include every value of
-- @e@; this means that the potential result could be either a 'Left' (making it
-- a choice of type @e@) or a 'Right' (making it a choice of type @a@).
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (ExceptT e m) where
  type SelectType (ExceptT e m) = SelectType m
  select = lift . select

-- | @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (IdentityT m) where
  type SelectType (IdentityT m) = SelectType m
  select = lift . select

-- | Provides a read-only environment of type @r@ to the \'strategy\' function.
-- However, the \'ranking\' function (or more accurately, representation) has no
-- access to @r@. Put another way, you can influence what values get chosen by
-- changing @r@, but not how solutions are ranked.
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (ReaderT r m) where
  type SelectType (ReaderT r m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the state: the \'ranking\' function can /see/ a value of
-- type @s@, but not modify it. Effectively, can be thought of as \'extending\'
-- the \'ranking\' by all values in @s@, but /which/ @s@ gets given to any rank
-- calls is predetermined by the \'outer state\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (LazyState.StateT s m) where
  type SelectType (LazyState.StateT s m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the state: the \'ranking\' function can /see/ a value of
-- type @s@, but not modify it. Effectively, can be thought of as \'extending\'
-- the \'ranking\' by all values in @s@, but /which/ @s@ gets given to any rank
-- calls is predetermined by the \'outer state\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (StrictState.StateT s m) where
  type SelectType (StrictState.StateT s m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the writer: the \'ranking\' function can see the value
-- that's been accumulated (of type @w@), but can't add anything to the log.
-- Effectively, can be thought of as \'extending\' the \'ranking\' by all values
-- of @w@, but /which/ @w@ gets given to any rank calls is predetermined by the
-- \'outer writer\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (CPSWriter.WriterT w m) where
  type SelectType (CPSWriter.WriterT w m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the writer: the \'ranking\' function can see the value
-- that's been accumulated (of type @w@), but can't add anything to the log.
-- Effectively, can be thought of as \'extending\' the \'ranking\' by all values
-- of @w@, but /which/ @w@ gets given to any rank calls is predetermined by the
-- \'outer writer\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m, Monoid w) => MonadSelect (LazyWriter.WriterT w m) where
  type SelectType (LazyWriter.WriterT w m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the writer: the \'ranking\' function can see the value
-- that's been accumulated (of type @w@), but can't add anything to the log.
-- Effectively, can be thought of as \'extending\' the \'ranking\' by all values
-- of @w@, but /which/ @w@ gets given to any rank calls is predetermined by the
-- \'outer writer\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m, Monoid w) => MonadSelect (StrictWriter.WriterT w m) where
  type SelectType (StrictWriter.WriterT w m) = SelectType m
  select = lift . select

-- | A combination of an \'outer\' 'ReaderT', 'WriterT' and 'StateT'. In short,
-- you get a value of type @r@ which can influence what gets picked, but not how
-- anything is ranked, and the \'ranking\' function gets access to an @s@ and a
-- @w@, but can modify neither.
--
-- @since 0.4.0.0
instance (MonadSelect m) => MonadSelect (CPSRWS.RWST r w s m) where
  type SelectType (CPSRWS.RWST r w s m) = SelectType m
  select = lift . select

-- | A combination of an \'outer\' 'ReaderT', 'WriterT' and 'StateT'. In short,
-- you get a value of type @r@ which can influence what gets picked, but not how
-- anything is ranked, and the \'ranking\' function gets access to an @s@ and a
-- @w@, but can modify neither.
--
-- @since 0.4.0.0
instance (MonadSelect m, Monoid w) => MonadSelect (LazyRWS.RWST r w s m) where
  type SelectType (LazyRWS.RWST r w s m) = SelectType m
  select = lift . select

-- | A combination of an \'outer\' 'ReaderT', 'WriterT' and 'StateT'. In short,
-- you get a value of type @r@ which can influence what gets picked, but not how
-- anything is ranked, and the \'ranking\' function gets access to an @s@ and a
-- @w@, but can modify neither.
--
-- @since 0.4.0.0
instance (MonadSelect m, Monoid w) => MonadSelect (StrictRWS.RWST r w s m) where
  type SelectType (StrictRWS.RWST r w s m) = SelectType m
  select = lift . select

-- | \'Readerizes\' the accumulator: the \'ranking\' function can see the value
-- that has been accumulated (of type @w@), but can't add anything to it.
-- Effectively, can be thought of as \'extending\' the \'ranking\' by all values
-- of @w@, but /which/ @w@ gets given to any rank calls is predetermined by the
-- \'outer accumulation\' (and cannot change).
--
-- @since 0.4.0.0
instance (MonadSelect m, Monoid w) => MonadSelect (AccumT w m) where
  type SelectType (AccumT w m) = SelectType m
  select = lift . select
