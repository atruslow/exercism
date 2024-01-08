module DNA (toRNA) where

datagit  DNA =
    | G
    | C
    | T
    | A

Type RNA =
    | C




- `G` -> `C`
- `C` -> `G`
- `T` -> `A`
- `A` -> `U`

toRNA :: String -> Either Char String
toRNA xs = error "You need to implement this function."
