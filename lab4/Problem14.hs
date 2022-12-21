replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x


duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) =  [x | x <- replicate 2 x] ++ duplicate xs