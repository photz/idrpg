module SymTab

import Data.SortedMap

-- symbol table that uses a stack internally



data Entry = VarDecl | FunDecl

new : SortedMap String Entry
new = empty

incrNestLevel : List (SortedMap String Entry)
              -> List (SortedMap String Entry)
incrNestLevel xs = new::xs

decrNestLevel : List (SortedMap String Entry)
              -> Either String (List (SortedMap String Entry))
decrNestLevel Nil = Left ""
decrNestLevel (m::ms) = Right ms

enter : List (SortedMap String Entry)
      -> String
      -> Entry
      -> Either String (List (SortedMap String Entry))
enter Nil _ _ = Left "empty symbol table"
enter (m::ms) id entry =
  case lookup id m of
    Just _ => Left "this id is taken"
    Nothing =>
      Right ((insert id entry m)::ms)


retrieve : List (SortedMap String Entry)
         -> String
         -> Maybe Entry
retrieve Nil id = Nothing
retrieve (m::ms) id =
  case lookup id m of
    Nothing => retrieve ms id
    Just entry => Just entry
