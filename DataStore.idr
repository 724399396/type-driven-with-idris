module Datastore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString | SInt | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

export
record DataStore (schema : Schema) where
       constructor MkData
       size : Nat
       items : Vect size (SchemaType schema)

export
empty : DataStore schema
empty = MkData 0 []

export
addToStore : (value: SchemaType schema) -> (store : DataStore schema) -> DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

public export
data StoreView : DataStore schema -> Type where
     SNil : StoreView empty
     SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (x :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelp items

-- data Command : Schema -> Type where
--              SetSchema : (newschema : Schema) -> Command schema
--              Add : SchemaType schema -> Command schema
--              Get : Integer -> Command schema
--              Quit : Command schema

-- parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
-- parsePrefix SString input = getQuoted (unpack input)
--             where
--               getQuoted : List Char -> Maybe (String, String)
--               getQuoted ('"' :: xs) = case span (/= '"') xs of
--                                                     (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
--                                                     _ => Nothing
--               getQuoted _ = Nothing
-- parsePrefix SInt input = case span isDigit input of
--                               ("", rest) => Nothing
--                               (num, rest) => Just (cast num, ltrim rest)
-- parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
--                                                Nothing => Nothing
--                                                Just (l_val, input') =>
--                                                     case parsePrefix schemar input' of
--                                                          Nothing => Nothing
--                                                          Just (r_val, input'') =>
--                                                               Just ((l_val, r_val), input'')

-- parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
-- parseBySchema schema input = case parsePrefix schema input of
--                                   Just (res, "") => Just res
--                                   Just _ => Nothing
--                                   Nothing => Nothing

-- parseSchema : List String -> Maybe Schema
-- parseSchema ("String" :: xs) = case xs of
--                                     [] => Just SString
--                                     _ => case parseSchema xs of
--                                               Nothing => Nothing
--                                               Just xs_sch => Just (SString .+. xs_sch)
-- parseSchema ("Int" :: xs) = case xs of
--                                  [] => Just SInt
--                                  _ => case parseSchema xs of
--                                            Nothing => Nothing
--                                            Just xs_sch => Just (SInt .+. xs_sch)
-- parseSchema _ = Nothing


-- parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
-- parseCommand schema "add" str = case parseBySchema schema str of
--                                      Nothing => Nothing
--                                      Just restok => Just (Add restok)
-- parseCommand schema "get" val = case all isDigit (unpack val) of
--                               False => Nothing
--                               True => Just (Get (cast val))
-- parseCommand schema "quit" "" = Just Quit
-- parseCommand schema "schema" rest = do schemaok <- parseSchema (words rest)
--                                        Just (SetSchema schemaok)

-- parseCommand _ _ _ = Nothing

-- parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
-- parse schema input = case span (/= ' ') input of
--                    (cmd, args) => parseCommand schema cmd (ltrim args)


-- display :  SchemaType schema -> String
-- display {schema = SString} item = item
-- display {schema = SInt} item = show item
-- display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

-- getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
-- getEntry pos store = let store_items = items store in
--                          case integerToFin pos (size store) of
--                               Nothing => Just ("Out of range\n", store)
--                               Just id => Just (display (index id store_items) ++ "\n", store)

-- setSchema : (store : DataStore) -> Schema -> Maybe DataStore
-- setSchema store schema = case size store of
--                               Z => Just (MkData schema _ [])
--                               S k => Nothing

-- processInput : DataStore -> String -> Maybe (String, DataStore)
-- processInput store inp = case parse (schema store) inp of
--                               Nothing => Just ("Invalid command\n", store)
--                               (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
--                               (Just (SetSchema schema')) =>
--                                     case setSchema store schema' of
--                                          Nothing => Just ("Can't update schema\n", store)
--                                          Just store' => Just ("OK\n", store')
--                               (Just (Get pos)) => getEntry pos store
--                               (Just Quit) => Nothing

-- main : IO ()
-- main = replWith (MkData SString _ []) "Command: " processInput

