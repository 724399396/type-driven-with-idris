%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = putStrLn "Out of fuel"

totalREPL : (prompt : String) -> (action: String -> String) -> InfIO
totalREPL prompt action = do putStr prompt
                             input <- getLine
                             putStrLn (action input)
                             totalREPL prompt action

partial
forever : Fuel
forever = More forever
