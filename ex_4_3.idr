module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
           where
            addToData : Vect old String -> Vect (S old) String
            addToData [] = [newitem]
            addToData (item' :: items') = item' :: addToData items'

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store
                              (Just Quit) => Nothing
                              (Just Size) => Just (show (size store) ++ "\n", store)
                              (Just (Search content)) =>  let storeSize = size store
                                                              withIndexStore = zip (vectIndex storeSize) (items store) in
                                                              Just (filterMatch content withIndexStore , store)
                                                              -- Just (Vect.foldl1 ?action $ map (\(id,item) => id ++ ": " ++ item ++ "\n") $ Vect.filter (isInfixOf content . snd) withIndexStore , store)
                         where
                             vectIndex : (n : Nat) -> Vect n Nat
                             vectIndex Z = []
                             vectIndex (S n) = n :: vectIndex n
                             filterMatch : String -> Vect n (Nat, String) -> String
                             filterMatch _ [] = ""
                             filterMatch content ((idx,x) :: xs) = case isInfixOf content x of
                                                                        False => filterMatch content xs
                                                                        True => (show idx) ++ ": " ++ x ++ "\n" ++ (filterMatch content xs)



main : IO ()
main = replWith (MkData _ []) "Command: " processInput
