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

sublists :: Int -> [Int] -> [(Int, [Int])]
sublists n xs = take (length xs - n + 1) (sublists' n xs)
    where sublists' _ [] = [(0, [])]
          sublists' 1 [x] = [(x, [x])]
          sublists' n xs@(_:rest) = ((sum' xs), take n xs) : sublists' n rest

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ (subsets xs)

sumofsets :: [Int] -> [(Int, [Int])]
sumofsets [] = []
sumofsets xs = (nsubsets (length xs) xs)
    where nsubsets 1 xs = sublists 1 xs
          nsubsets n xs = (sublists n xs) ++ nsubsets (n-1) xs

ksmallest :: [Int] -> Int -> [[Int]]
ksmallest [] k = []
ksmallest xs k = []

main :: IO ()
main = print(sumofsets list)