list :: [Int]
list = [-1, 2, -3, 4, -5]

list2 :: [[Int]]
list2 = [[1,2,3], [4,5,6]]

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y : ys) = if x <= y then x : y : ys else y : ins x ys 

iSort :: [Int] -> [Int]
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

ksmallest :: [Int] -> Int -> [[Int]]
ksmallest [] k = []
ksmallest xs k = []

main :: IO ()
main = print(sumofsets list)