fac :: (Eq t, Num t) => t -> t
fac 0 = 1
fac n = n * fac (n-1)

main :: IO ()
main = do {putStrLn "give a number pls";
        x <- readLn;
        print (fac x)}