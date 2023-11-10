verifySpacing isbn
    | ' ' `elem` isbn = 1
    | '-' `elem` isbn = 2
    | otherwise = 0

removeSpaces isbn spaced
    | spaced == 1 = filter (/= ' ') isbn
    | spaced == 2 = filter (/= '-') isbn
    | otherwise = isbn


generaISBN isbn = removeSpaces isbn (verifySpacing isbn)

add10 isbn = isbn ++ "10"

checkLastDigit isbn
    | last isbn == 'X' = add10 (init isbn)

finalISBN isbn = checkLastDigit (generaISBN isbn)
