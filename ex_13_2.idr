import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
     Push : Integer -> StackCmd () height (S height)
     Pop : StackCmd Integer (S height) height
     Top : StackCmd Integer (S height) (S height)
     GetStr : StackCmd String height height
     PutStr : String -> StackCmd () height height
     Pure : ty -> StackCmd ty height height
     Bind : StackCmd a height1 height2 ->
             (a -> StackCmd b height2 height3) ->
             StackCmd b height1 height3

namespace StackDo
          (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3
          (>>=) = Bind

runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (Bind cmd next) = do (cmdRes, newStk) <- runStack stk cmd
                                  runStack newStk (next cmdRes)

data StackIO : Nat -> Type where
     Do : StackCmd a height1 height2 ->
          (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackIODo
          (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1
          (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do (res, newStk) <- runStack stk c
                                  run fuel newStk (f res)
run Dry stk p = pure ()

data StkInput = Number Integer
              | Add
              | Sub
              | Mul
              | Neg
              | Discard
              | Dup

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Sub
strToInput "multiply" = Just Mul
strToInput "negate" = Just Neg
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Dup
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

doSub : StackCmd () (S (S height)) (S height)
doSub = do val1 <- Pop
           val2 <- Pop
           Push (val2 - val1)

doMul : StackCmd () (S (S height)) (S height)
doMul = do val1 <- Pop
           val2 <- Pop
           Push (val1 * val2)

doNeg : StackCmd () (S height) (S height)
doNeg = do val <- Pop
           Push (0 - val)

mutual
  tryAdd : StackIO height
  tryAdd {height = (S (S h))} = do doAdd
                                   result <- Top
                                   PutStr (show result ++ "\n")
                                   stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  trySub : StackIO height
  trySub {height = (S (S h))} = do doSub
                                   result <- Top
                                   PutStr (show result ++ "\n")
                                   stackCalc
  trySub = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  tryMul : StackIO height
  tryMul {height = (S (S h))} = do doMul
                                   result <- Top
                                   PutStr (show result ++ "\n")
                                   stackCalc
  tryMul = do PutStr "Fewer than two items on the stack\n"
              stackCalc

  tryNeg : StackIO height
  tryNeg {height = (S h)} = do doNeg
                               result <- Top
                               PutStr (show result ++ "\n")
                               stackCalc
  tryNeg = do PutStr "Fewer than one items on the stack\n"
              stackCalc

  tryDiscard : StackIO height
  tryDiscard {height = (S h)} = do val <- Pop
                                   PutStr ("Discarded " ++ show val ++ "\n")
                                   stackCalc
  tryDiscard = do PutStr "No items to discarded"
                  stackCalc

  tryDup : StackIO height
  tryDup {height = (S h)} = do val <- Top
                               Push val
                               PutStr ("Duplicated " ++ show val ++ "\n")
                               stackCalc

  tryDup = do PutStr "No items to discarded"
              stackCalc

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input\n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryAdd
                      Just Sub => trySub
                      Just Mul => tryMul
                      Just Neg => tryNeg
                      Just Discard => tryDiscard
                      Just Dup => tryDup

  partial
  main : IO ()
  main = run forever [] stackCalc
