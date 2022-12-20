main = do
    print $ insert 'd' "abcd" 3


insert :: a -> [a] -> Int -> [a]
insert s xs 1 = s:xs
insert s (x:xs) m = x: insert s xs (m-1)
    