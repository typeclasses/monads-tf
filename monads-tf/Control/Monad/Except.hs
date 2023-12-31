{- |
Module      :  Control.Monad.Except
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
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

[Example type:] @'Data.Either' String a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://web.cecs.pdx.edu/~andy/>)
-}
module Control.Monad.Except (
    -- * Monads with error handling
    MonadError(..),
    tryError,
    withError,
    -- * The ExceptT monad transformer
    ExceptT(..),
    runExceptT,
    mapExceptT,
    withExceptT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Example 1: Custom Error Data Type
    -- $customErrorExample

    -- * Example 2: Using ExceptT Monad Transformer
    -- $ExceptTExample
  ) where

import Control.Monad.Except.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, mapExceptT, withExceptT)

import Control.Monad
import Control.Monad.Fix

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: (MonadError m) => m a -> m (Either (ErrorType m) a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | 'MonadError' analogue to the 'withExceptT' function.
-- Modify the value (but not the type) of an error.
-- The type is fixed because of the 'ErrorType' type family.
withError :: (MonadError m) => (ErrorType m -> ErrorType m) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

{- $customErrorExample
Here is an example that demonstrates the use of a custom 'Error' data type with
the 'throwError' and 'catchError' exception mechanism from 'MonadError'.
The example throws an exception if the user enters an empty string
or a string longer than 5 characters. Otherwise it prints length of the string.

>import Control.Monad.Except
>
>-- This is the type to represent length calculation error.
>data LengthError = EmptyString  -- Entered string was empty.
>          | StringTooLong Int   -- A string is longer than 5 characters.
>                                -- Records a length of the string.
>          | OtherError String   -- Other error, stores the problem description.
>
>-- Converts LengthError to a readable message.
>instance Show LengthError where
>  show EmptyString = "The string was empty!"
>  show (StringTooLong len) =
>      "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
>  show (OtherError msg) = msg
>
>-- For our monad type constructor, we use Either LengthError
>-- which represents failure using Left LengthError
>-- or a successful result of type a using Right a.
>type LengthMonad = Either LengthError
>
>main = do
>  putStrLn "Please enter a string:"
>  s <- getLine
>  reportResult (calculateLengthOrFail s)
>
>-- Attempts to calculate length and throws an error if the provided string is
>-- empty or longer than 5 characters.
>-- The processing is done in Either monad.
>calculateLengthOrFail :: String -> LengthMonad Int
>calculateLengthOrFail [] = throwError EmptyString
>calculateLengthOrFail s | len > 5 = throwError (StringTooLong len)
>                        | otherwise = return len
>  where len = length s
>
>-- Prints result of the string length calculation.
>reportResult :: LengthMonad Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}

{- $ExceptTExample
@'ErrorT'@ monad transformer can be used to add error handling to another monad.
Here is an example how to combine it with an @IO@ monad:

>import Control.Monad.Except
>
>-- An IO monad which can return String failure.
>-- It is convenient to define the monad type of the combined monad,
>-- especially if we combine more monad transformers.
>type LengthMonad = ExceptT String IO
>
>main = do
>  -- runExceptT removes the ExceptT wrapper
>  r <- runExceptT calculateLength
>  reportResult r
>
>-- Asks user for a non-empty string and returns its length.
>-- Throws an error if user enters an empty string.
>calculateLength :: LengthMonad Int
>calculateLength = do
>  -- all the IO operations have to be lifted to the IO monad in the monad stack
>  liftIO $ putStrLn "Please enter a non-empty string: "
>  s <- liftIO getLine
>  if null s
>    then throwError "The string was empty!"
>    else return $ length s
>
>-- Prints result of the string length calculation.
>reportResult :: Either String Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}
