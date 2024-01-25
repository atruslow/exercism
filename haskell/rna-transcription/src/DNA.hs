module DNA (toRNA) where

mapDNAtoRNA :: Char -> Either Char Char
mapDNAtoRNA 'G' = Right 'C'
mapDNAtoRNA 'C' = Right 'G'
mapDNAtoRNA 'T' = Right 'A'
mapDNAtoRNA 'A' = Right 'U'
mapDNAtoRNA x = Left x

toRNA :: String -> Either Char String
toRNA = mapM mapDNAtoRNA
    
    
    
    
    -- if all isValidDNA xs
    -- then Right  (map mapDNAtoRNA xs)
    -- else Left (find( not (isValidDNA ) ))
