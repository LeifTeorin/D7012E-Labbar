fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 1
fib 1 = 1

fib n = fib(n-1) + fib(n-2)

main :: IO ()
main = do {putStrLn "give a number pls";
        x <- readLn;
        print (fib x)}