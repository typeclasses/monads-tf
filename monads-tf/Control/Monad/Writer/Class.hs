-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Class (
    MonadWriter(..),
    listens,
    censor,
  ) where

import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (
        RWST, tell, listen, pass)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (
        RWST, tell, listen, pass)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (
        WriterT, tell, listen, pass)
import qualified Control.Monad.Trans.Writer.Strict as Strict (
        WriterT, tell, listen, pass)
import Control.Monad.Trans (lift)

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
    type WriterType m
    tell   :: WriterType m -> m ()
    listen :: m a -> m (a, WriterType m)
    pass   :: m (a, WriterType m -> WriterType m) -> m a

listens :: (MonadWriter m) => (WriterType m -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

censor :: (MonadWriter m) => (WriterType m -> WriterType m) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

instance (Monoid w, Monad m) => MonadWriter (Lazy.WriterT w m) where
    type WriterType (Lazy.WriterT w m) = w
    tell   = Lazy.tell
    listen = Lazy.listen
    pass   = Lazy.pass

instance (Monoid w, Monad m) => MonadWriter (Strict.WriterT w m) where
    type WriterType (Strict.WriterT w m) = w
    tell   = Strict.tell
    listen = Strict.listen
    pass   = Strict.pass

instance (Monoid w, Monad m) => MonadWriter (LazyRWS.RWST r w s m) where
    type WriterType (LazyRWS.RWST r w s m) = w
    tell   = LazyRWS.tell
    listen = LazyRWS.listen
    pass   = LazyRWS.pass

instance (Monoid w, Monad m) => MonadWriter (StrictRWS.RWST r w s m) where
    type WriterType (StrictRWS.RWST r w s m) = w
    tell   = StrictRWS.tell
    listen = StrictRWS.listen
    pass   = StrictRWS.pass

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Error e, MonadWriter m) => MonadWriter (ErrorT e m) where
    type WriterType (ErrorT e m) = WriterType m
    tell   = lift . tell
    listen = Error.liftListen listen
    pass   = Error.liftPass pass

instance (MonadWriter m) => MonadWriter (ExceptT e m) where
    type WriterType (ExceptT e m) = WriterType m
    tell   = lift . tell
    listen = Except.liftListen listen
    pass   = Except.liftPass pass

instance (MonadWriter m) => MonadWriter (IdentityT m) where
    type WriterType (IdentityT m) = WriterType m
    tell   = lift . tell
    listen = Identity.mapIdentityT listen
    pass   = Identity.mapIdentityT pass

instance (MonadWriter m) => MonadWriter (MaybeT m) where
    type WriterType (MaybeT m) = WriterType m
    tell   = lift . tell
    listen = Maybe.liftListen listen
    pass   = Maybe.liftPass pass

instance (MonadWriter m) => MonadWriter (ReaderT r m) where
    type WriterType (ReaderT r m) = WriterType m
    tell   = lift . tell
    listen = mapReaderT listen
    pass   = mapReaderT pass

instance (MonadWriter m) => MonadWriter (Lazy.StateT s m) where
    type WriterType (Lazy.StateT s m) = WriterType m
    tell   = lift . tell
    listen = Lazy.liftListen listen
    pass   = Lazy.liftPass pass

instance (MonadWriter m) => MonadWriter (Strict.StateT s m) where
    type WriterType (Strict.StateT s m) = WriterType m
    tell   = lift . tell
    listen = Strict.liftListen listen
    pass   = Strict.liftPass pass
