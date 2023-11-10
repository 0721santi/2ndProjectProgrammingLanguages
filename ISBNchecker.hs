verifySpacing isbn
  | ' ' `elem` isbn = 1
  | '-' `elem` isbn = 2
  | otherwise = 0

removeSpaces isbn spaced
  | spaced == 1 = filter (/= ' ') isbn
  | spaced == 0 = filter (/= '-') isbn
  | otherwise = isbn
