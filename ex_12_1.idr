import Control.Monad.State

update : (stateType -> stateType) -> State stateType ()
update f = do old <- get
              put (f old)

increase : Nat -> State Nat ()
increase x = update (+x)

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left _ right) = do countEmpty left
                                    countEmpty right


countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = update (\(a,b) => (a+1,b))
countEmptyNode (Node left _ right) = do update (\(a,b) => (a,b+1))
                                        countEmptyNode left
                                        countEmptyNode right
