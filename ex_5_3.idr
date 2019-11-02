import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "") then pure []
                 else do xs <- readToBlank
                         pure (x :: xs)

readAndSave : IO ()
readAndSave = do list <- readToBlank
                 filename <- getLine
                 Right _ <- writeFile filename (show list) | Left err => putStrLn (show err)
                 pure ()

readVect : File -> IO (len ** Vect len String)
readVect file = do end <- fEOF file
                   if end
                   then pure (_ ** [])
                   else do Right x <- fGetLine file
                                      | Left err => do putStrLn (show err)
                                                       pure (_ ** [])
                           (_ ** xs) <- readVect file
                           pure (_ ** (x :: xs))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
                          Right f <- openFile filename Read | Left err => do putStrLn (show err)
                                                                             pure (_ ** [])
                          vect <- readVect f
                          closeFile f
                          pure vect


