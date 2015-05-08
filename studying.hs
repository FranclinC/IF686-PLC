-- This file is to practice haskell

--Vendas semanais
vendas :: Int -> Int
vendas 0 = 0
vendas n
 | n == 1 = 5
 | n == 2 = 7
 | n == 3 = 24
 | otherwhise = 50