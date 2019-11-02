printLonger : IO ()
printLonger = do putStr "First string: "
                 x <- getLine
                 putStr "Second string: "
                 y <- getLine
                 let firstLen = length x
                 let secondLen = length y
                 if firstLen > secondLen
                 then putStrLn (show firstLen)
                 else putStrLn (show secondLen)


printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
                     getLine >>= \x =>
                     putStr "Second string: " >>= \_ =>
                     getLine >>= \y =>
                     let firstLen = length x
                         secondLen = length y in
                         if firstLen > secondLen
                         then putStrLn (show firstLen)
                         else putStrLn (show secondLen)
