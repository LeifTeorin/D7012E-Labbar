import Distribution.Simple.Utils (xargs)
list :: [Int]
list = [-1, 2, -3, 4, -5]

list2 :: [[Int]]
list2 = [[1,2,3], [4,5,6]]

ins :: (Int, [Int]) -> [(Int, [Int])] -> [(Int, [Int])]
ins x [] = [x]
ins (x, lst) ((y, lst2) : ys) = if x <= y then (x, lst) : (y, lst2) : ys else (y, lst2) : ins (x, lst) ys 

iSort :: [(Int, [Int])] -> [(Int, [Int])]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

sum2 :: [[Int]] -> [(Int, [Int])]
sum2 [] = [(0, [])]
sum2 (x: xs) = [(sum' x, x)] ++ sum2 xs

sublists :: Int -> [Int] -> [[Int]]
sublists n xs = take (length xs - n + 1) (sublists' n xs)
    where sublists' _ [] = [[]]
          sublists' n xs@(_:rest) = (take n xs) : sublists' n rest

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ (subsets xs)

sumofsets :: [Int] -> [(Int, [Int])]
sumofsets [] = []
sumofsets xs = (sum2 (nsubsets (length xs) xs))
    where nsubsets 1 xs = sublists 1 xs
          nsubsets n xs = (sublists n xs) ++ nsubsets (n-1) xs

ksmallest :: [Int] -> Int -> [(Int, [Int])]
ksmallest [] k = []
ksmallest xs k = pickfromsortedK (iSort (sumofsets xs)) k
    where pickfromsortedK (x:xs) 1 = [x]
          pickfromsortedK (x:xs) n = [x] ++ pickfromsortedK xs (n-1)

smallestKset :: [Int] -> Int -> IO ()
smallestKset _ 0 = putStr("no sets to pick :,(")
smallestKset xs k = putStr(smallestKstring (ksmallest xs k) k)
    where smallestKstring ((size, lst):xs) k = "size: " ++ show(size) ++ "  " ++ "subset: " ++ show(lst) ++ "\n" ++ (smallestKstring xs (k-1))
          smallestKstring _ 0 = "\n"
          smallestKstring [] _ = "\n"

main :: IO ()
main = smallestKset list 0