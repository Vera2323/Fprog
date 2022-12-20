main = do
    print $ reversed [1,3,6]


reversed :: [a] -> [a]
reversed [] = []
reversed (x:xs) = reversed xs ++ [x]