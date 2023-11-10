verifySpacing isbn
  | ' ' `elem` isbn = 1
  | '-' `elem` isbn = 2
  | otherwise = 0

removeSpaces isbn spaced
  | spaced == 1 = filter (/= ' ') isbn
  | spaced == 2 = filter (/= '-') isbn
  | otherwise = isbn
  

generaISBN isbn = removeSpaces isbn (verifySpacing isbn)

-- checkLastDigit isbn
--     if(last isbn == 'X')
--         then init isbn