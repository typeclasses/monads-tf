import Control.Monad.Except
import Control.Monad.IO.Class

-- An IO monad which can return String failure.
-- It is convenient to define the monad type of the combined monad,
-- especially if we combine more monad transformers.
type LengthMonad = ExceptT String IO

main :: IO ()
main = do
  -- runExceptT removes the ExceptT wrapper
  r <- runExceptT calculateLength
  reportResult r

-- Asks user for a non-empty string and returns its length.
-- Throws an error if user enters an empty string.
calculateLength :: LengthMonad Int
calculateLength = do
  -- all the IO operations have to be lifted to the IO monad in the monad stack
  liftIO $ putStrLn "Please enter a non-empty string: "
  s <- liftIO getLine
  if null s
    then throwError "The string was empty!"
    else return $ length s

-- Prints result of the string length calculation.
reportResult :: Either String Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
