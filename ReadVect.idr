import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

data VectUnkown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnkown a

readVect : IO (VectUnkown String)
readVect = do x <- getLine
              if (x == "")
                 then pure (MkVect _ [])
                 else do MkVect _ xs <- readVect
                         pure (MkVect _ (x :: xs))

printVect : Show a => VectUnkown a -> IO ()
printVect (MkVect len xs)
          = putStrLn (show xs ++ " (length " ++ show len ++ ")")
