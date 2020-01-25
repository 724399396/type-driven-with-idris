data GuessCmd : Type -> Nat -> Nat -> Type where
     Try : Integer -> GuessCmd Ordering (S next) next
     Pure : ty -> GuessCmd ty state state
     (>>=) : GuessCmd a state1 state2 ->
             (a -> GuessCmd b state2 state3) ->
             GuessCmd b state1 state3

threeGusses : GuessCmd () 3 0
threeGusses = do Try 10
                 Try 20
                 Try 15
                 Pure ()

-- noGuesses : GuessCmd () 0 0
-- noGuesses = do Try 10
--                Pure ()
