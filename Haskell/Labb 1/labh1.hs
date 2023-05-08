-- Hjalmar Olofsson Utsi

list :: [Int]
list = [-1, 2, -3, 4, -5]

listbig :: [Int]
listbig = map(\x -> x * (-1)^x) [1..99]

list2 :: [[Int]]
list2 = [[1,2,3], [4,5,6]]

ins :: (Int, Int, Int, [Int]) -> [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
ins x [] = [x]
ins (xsize, xi, xj, lst) ((ysize, yi, yj, lst2) : ys) = if xsize <= ysize then (xsize, xi, xj, lst) : (ysize, yi, yj, lst2) : ys else (ysize, yi, yj, lst2) : ins (xsize, xi, xj, lst) ys 

iSort :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

sum2 :: [(Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
sum2 [] = [(0, -1, -1, [])]
sum2 ((i, j, x): xs) = [(sum' x, i, j, x)] ++ sum2 xs

sublists :: Int -> [Int] -> [(Int, Int, [Int])]
sublists n xs = take (length xs - n + 1) (sublists' 0 n xs)
    where sublists' i n (xs:rest) = (i, (i + (n-1)), (take n (xs:rest))) : sublists' (i+1) n rest

sumofsets :: [Int] -> [(Int, Int, Int, [Int])]
sumofsets [] = []
sumofsets xs = (sum2 (nsubsets (length xs) xs))
    where nsubsets 1 xs = sublists 1 xs
          nsubsets n xs = (sublists n xs) ++ nsubsets (n-1) xs

ksmallest :: [Int] -> Int -> [(Int, Int, Int, [Int])]
ksmallest [] k = []
ksmallest xs k = take k (iSort (sumofsets xs))
    where pickfromsortedK (x:xs) 1 = [x]
          pickfromsortedK x n = take n x

smallestKset :: [Int] -> Int -> IO ()
smallestKset _ 0 = putStr("no sets to pick :,(")
smallestKset xs k = putStr("Entire list: " ++ show(xs) ++ "     k = " ++ show(k) ++ "\n" ++ smallestKstring (ksmallest xs k) k)
    where smallestKstring ((size, i, j, lst):xs) k = "size: " ++ show(size) ++ "  " ++ "i: " ++ show(i) ++ "    j: " ++ show(j) ++ "    subset: " ++ show(lst) ++ "\n" ++ (smallestKstring xs (k-1))
          smallestKstring _ 0 = "\n"
          smallestKstring [] _ = "\n"

main :: IO ()
main = smallestKset listbig 5