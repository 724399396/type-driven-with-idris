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

partial
forever : Fuel
forever = More forever


greet : InfIO
greet = do putstr "Enter your name: "
           name <- getLine
           putStrLn ("Hello " ++ name)
           greet
