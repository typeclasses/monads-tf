{- |
Module      :  Control.Monad.Error.Class
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
               (c) Edward Kmett 2012
License     :  BSD-style (see the file LICENSE)

Maintainer  :  ross@soi.city.ac.uk
Stability   :  experimental
Portability :  non-portable (type families)

[Computation type:] Computations which may fail or throw exceptions.

[Binding strategy:] Failure records information about the cause\/location
of the failure. Failure values bypass the bound function,
other values are used as inputs to the bound function.

[Useful for:] Building computations from sequences of functions that may fail
or using exception handling to structure error handling.

[Zero and plus:] Zero is represented by an empty error and the plus operation
executes its second argument if the first fails.

[Example type:] @'Either' 'String' a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://web.cecs.pdx.edu/~andy/>)
-}
module Control.Monad.Error.Class (
    MonadError(..),
    liftEither,
    tryError,
    withError,
    handleError,
    mapError,
    modifyError,
  ) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as ExceptT (catchE, runExceptT, throwE)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Identity qualified as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Maybe qualified as Maybe
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.RWS.Lazy qualified as LazyRWS
import Control.Monad.Trans.RWS.Strict qualified as StrictRWS
import Control.Monad.Trans.State.Lazy qualified as LazyState
import Control.Monad.Trans.State.Strict qualified as StrictState
import Control.Monad.Trans.Writer.Lazy qualified as LazyWriter
import Control.Monad.Trans.Writer.Strict qualified as StrictWriter
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Accum qualified as Accum
import Control.Monad.Trans.RWS.CPS qualified as CPSRWS
import Control.Monad.Trans.Writer.CPS qualified as CPSWriter
import Control.Monad.Trans.Class (lift)
import Control.Exception (IOException, catch)

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own error type and\/or use a monad type constructor
other than @'Either' 'String'@ or @'Either' 'IOError'@.
In these cases you will have to explicitly define instances of the 'MonadError'
class.
(If you are using the deprecated "Control.Monad.Error" or
"Control.Monad.Trans.Error", you may also have to define an 'Error' instance.)
-}
class (Monad m) => MonadError m where
    -- | The type of the throwable error.
    type ErrorType m

    -- | Is used within a monadic computation to begin exception processing.
    throwError :: ErrorType m -> m a

    {- |
    A handler function to handle previous errors and return to normal execution.
    A common idiom is:

    > do { action1; action2; action3 } `catchError` handler

    where the @action@ functions can call 'throwError'.
    Note that @handler@ and the do-block must have the same return type.
    -}
    catchError :: m a -> (ErrorType m -> m a) -> m a
    {-# MINIMAL throwError, catchError #-}

{- |
Lifts an @'Either' e@ into any @'MonadError' e@.

> do { val <- liftEither =<< action1; action2 }

where @action1@ returns an 'Either' to represent errors.

@since 0.4.0.0
-}
liftEither :: MonadError m => Either (ErrorType m) a -> m a
liftEither = either throwError pure

instance MonadError IO where
    type ErrorType IO = IOException
    throwError = ioError
    catchError = catch

{- | @since 0.4.0.0 -}
instance MonadError Maybe where
    type ErrorType Maybe = ()
    throwError ()        = Nothing
    catchError Nothing f = f ()
    catchError x       _ = x

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance MonadError (Either e) where
    type ErrorType (Either e) = e
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

instance (Monad m) => MonadError (ExceptT e m) where
    type ErrorType (ExceptT e m) = e
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

-- ---------------------------------------------------------------------------
-- Instances for other transformers

instance (MonadError m) => MonadError (IdentityT m) where
    type ErrorType (IdentityT m) = ErrorType m
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance (MonadError m) => MonadError (MaybeT m) where
    type ErrorType (MaybeT m) = ErrorType m
    throwError = lift . throwError
    catchError = Maybe.liftCatch catchError

instance (MonadError m) => MonadError (ReaderT r m) where
    type ErrorType (ReaderT r m) = ErrorType m
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (LazyRWS.RWST r w s m) where
    type ErrorType (LazyRWS.RWST r w s m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyRWS.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (StrictRWS.RWST r w s m) where
    type ErrorType (StrictRWS.RWST r w s m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictRWS.liftCatch catchError

instance (MonadError m) => MonadError (LazyState.StateT s m) where
    type ErrorType (LazyState.StateT s m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyState.liftCatch catchError

instance (MonadError m) => MonadError (StrictState.StateT s m) where
    type ErrorType (StrictState.StateT s m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictState.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (LazyWriter.WriterT w m) where
    type ErrorType (LazyWriter.WriterT w m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyWriter.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (StrictWriter.WriterT w m) where
    type ErrorType (StrictWriter.WriterT w m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictWriter.liftCatch catchError

-- | @since 0.4.0.0
instance (Monoid w, MonadError m) => MonadError (CPSRWS.RWST r w s m) where
    type ErrorType (CPSRWS.RWST r w s m) = ErrorType m
    throwError = lift . throwError
    catchError = CPSRWS.liftCatch catchError

-- | @since 0.4.0.0
instance (Monoid w, MonadError m) => MonadError (CPSWriter.WriterT w m) where
    type ErrorType (CPSWriter.WriterT w m) = ErrorType m
    throwError = lift . throwError
    catchError = CPSWriter.liftCatch catchError

-- | @since 0.4.0.0
instance
  ( Monoid w
  , MonadError m
  ) => MonadError (AccumT w m) where
    type ErrorType (AccumT w m) = ErrorType m
    throwError = lift . throwError
    catchError = Accum.liftCatch catchError

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
--
-- @since 0.4.0.0
tryError :: (MonadError m) => m a -> m (Either (ErrorType m) a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error. The type is
-- fixed because the type family @ErrorType@ is already defined
-- at @m@, If you need to change the type of @e@ use 'mapError'
-- or 'modifyError'.
--
-- @since 0.4.0.0
withError :: (MonadError m) => (ErrorType m -> ErrorType m) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

-- | As 'handle' is flipped 'Control.Exception.catch', 'handleError'
-- is flipped 'catchError'.
--
-- @since 0.4.0.0
handleError :: (MonadError m) => (ErrorType m -> m a) -> m a -> m a
handleError = flip catchError

-- | 'MonadError' analogue of the 'mapExceptT' function.  The
-- computation is unwrapped, a function is applied to the @Either@, and
-- the result is lifted into the second 'MonadError' instance.
--
-- @since 0.4.0.0
mapError :: (MonadError m, MonadError n) => (m (Either (ErrorType m) a) -> n (Either (ErrorType n) b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither

{- |
A different 'MonadError' analogue to the 'withExceptT' function.
Modify the value (and possibly the type) of an error in an @ExceptT@-transformed
monad, while stripping the @ExceptT@ layer.

This is useful for adapting the 'MonadError' constraint of a computation.

For example:

> data DatabaseError = ...
>
> performDatabaseQuery :: (MonadError m, ErrorType m ~ DatabaseError, ...) => m PersistedValue
>
> data AppError
>   = MkDatabaseError DatabaseError
>   | ...
>
> app :: (MonadError m, ErrorType m ~ AppError, ...) => m ()

Given these types, @performDatabaseQuery@ cannot be used directly inside
@app@, because the error types don't match. Using 'modifyError', an equivalent
function with a different error type can be constructed:

> performDatabaseQuery' :: (MonadError m, ErrorType m ~ AppError, ...) => m PersistedValue
> performDatabaseQuery' = modifyError MkDatabaseError performDatabaseQuery

Since the error types do match, @performDatabaseQuery'@ _can_ be used in @app@,
assuming all other constraints carry over.

This works by instantiating the @m@ in the type of @performDatabaseQuery@ to
@ExceptT DatabaseError m'@, which satisfies the @ErrorType m ~ AppError@
constraint. Immediately, the @ExceptT DatabaseError@ layer is unwrapped,
producing 'Either' a @DatabaseError@ or a @PersistedValue@. If it's the former,
the error is wrapped in @MkDatabaseError@ and re-thrown in the inner monad,
otherwise the result value is returned.

@since 0.4.0.0
-}
modifyError :: MonadError m => (e -> ErrorType m) -> ExceptT e m a -> m a
modifyError f m = ExceptT.runExceptT m >>= either (throwError . f) pure
