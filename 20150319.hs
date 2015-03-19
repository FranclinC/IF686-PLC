-- Aula 2. Primeiro slide

double :: [Int] -> [Int]
double [] = []
double (l:ls) = [l * 2] ++ double ls

member :: [Int] -> Int -> Bool
member [] x = False
member (l:ls) x 
 | l == x = True
 | otherwise = member ls x
 
-- Forma mais rápida 
digits :: String -> String
digits [] = []
digits (l:ls)
 | l >= '0' && l <= '9' = [l] ++ digits ls
 | otherwise = digits ls 
 
-- Forma lenta 
digits2 :: String -> String
digits2 [] = []
digits2 (l:ls)
 | l == '0' = "0" ++ digits2 ls
 | l == '1' = "1" ++ digits2 ls
 | l == '2' = "2" ++ digits2 ls
 | l == '3' = "3" ++ digits2 ls
 | l == '4' = "4" ++ digits2 ls
 | l == '5' = "5" ++ digits2 ls
 | l == '6' = "6" ++ digits2 ls
 | l == '7' = "7" ++ digits2 ls
 | l == '8' = "8" ++ digits2 ls
 | l == '9' = "9" ++ digits2 ls
 | otherwise = digits2 ls 
 

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs [] (x:xs) = [x] ++ (sumPairs [] xs)
sumPairs (l:ls) [] = [l] ++ (sumPairs ls [])
sumPairs (l:ls) (x:xs) = [l + x] ++ (sumPairs ls xs)