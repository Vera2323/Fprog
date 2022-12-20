data Answer = TwoRoots {x1, x2 :: Double} | OneRoot {x :: Double} | NoRoots



main :: IO ()
main = do 
    print "Enter a"
    a <- getLine
    print "Enter b"
    b <- getLine
    print "Enter c"
    c <- getLine
    print "Answer is"
    print . printAnswer $ solve (read a :: Double) (read b :: Double) (read c :: Double)


printAnswer :: Answer -> String
printAnswer NoRoots = "No Solutions"
printAnswer (OneRoot x) = "x is " ++ show x
printAnswer (TwoRoots x1 x2) ="x1 is " ++ show x1 ++ "; x2 is " ++ show x2

solve :: Double -> Double -> Double -> Answer
solve a b c
    | a == 0 || discr < 0 = NoRoots
    | discr > 0 = TwoRoots {x1 = x1, x2 = x2}
    | otherwise = OneRoot {x = x1}
    where
        discr = b^2 - 4 * a * c
        x1 = (-b - sqrt discr) / (2 * a)
        x2 = (-b + sqrt discr) / (2 * a)