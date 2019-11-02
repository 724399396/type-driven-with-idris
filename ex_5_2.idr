module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
      input <- getLine
      if all isDigit (unpack input)
      then pure (Just (cast input))
      else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
      Just inpNum <- readNumber | Nothing => do putStrLn "invalid number"
                                                guess target guesses
      if inpNum > target then (putStrLn "too hight" >>= \_ => guess target (S guesses))
                         else if inpNum < target
                         then (putStrLn "too low" >>= \_ => guess target (S guesses))
                         else (putStrLn "correct" >>= \_ => pure ())

main : IO ()
main = do
     t <- time
     guess (cast (t `mod` 100)) 0

my_repl : String -> (String -> String) -> IO ()
my_repl prompt action = do putStrLn prompt
                           x <- getLine
                           putStrLn (action x)
                           my_repl prompt action

my_replWith : a -> String -> (a -> String -> Maybe (String,a)) -> IO ()
my_replWith state prompt action = do putStrLn prompt
                                     x <- getLine
                                     case action state x of
                                          Just (toShow, newState)  => do putStrLn toShow
                                                                         my_replWith newState prompt action
                                          Nothing => pure ()


