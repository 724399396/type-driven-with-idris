import System
import Data.Primitives.Views

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

namespace CommandDO
          (>>=) : Command a -> (a -> Command b) -> Command b
          (>>=) = Bind

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDO
          (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
          (>>=) = Do


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit y) = pure (Just y)
run Dry (Do z f) = pure Nothing
run (More fuel) (Do z f) = do res <- runCommand z
                              run fuel (f res)


mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do PutStr ("Wrong, the answer is " ++
                                  show ans ++ "\n")
                            quiz nums score


  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score
     = do PutStr ("Score so far: " ++ show score ++ "\n")
          input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
          case input of
               Answer answer => if answer == num1 * num2
                                then correct nums score
                                else wrong nums (num1 * num2) score
               QuitCmd => Quit score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
(seed' `shiftR` 2) :: randoms seed'


arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
                   where bound : Int -> Int
                         bound num with (divides num 12)
                               bound ((12 * div) + rem) | (DivBy prf)= rem + 1


partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
