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

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

doAdd : StackCmd () (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

mutual
  tryAdd : StackIO height
  tryAdd {height = (S (S h))} = do doAdd
                                   result <- Top
                                   PutStr (show result ++ "\n")
                                   stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
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

  partial
  main : IO ()
  main = run forever [] stackCalc

