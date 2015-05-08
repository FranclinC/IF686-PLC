-- This file is to practice haskell

--Vendas semanais
vendas :: Int -> Int
vendas 0 = 0
vendas n
 | n == 1 = 5
 | n == 2 = 7
 | n == 3 = 24
 | otherwise = 50


-- venda total das semanas
totalVendas :: Int -> Int
totalVendas 0 = 0
totalVendas n = vendas n + totalVendas (n - 1)

func :: Int -> Int -> Int
func 0 0 = 1
func s n
 | (vendas n) == s = func s (n - 1) ++ 1
