main = putStr (show(removeAt 2 [1,2,3,5,1]))


removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
  [] -> error "index out of bounds"
  x : rest -> (x, front ++ rest)
  where
    (front, back) = splitAt (k - 1) xs