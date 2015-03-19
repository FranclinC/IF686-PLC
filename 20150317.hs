-- Q1
-- Definição da função, lembrar que todo tipo em Haskell começa com Maiusculo
vendas :: Int -> Int
-- Definição da função
vendas n = 1
 						
-- Definição de função						
fun :: Int -> Int -> Int
fun s n	|	n == 0 && s == vendas 0 = 1
		|	n == 0 = 0
		|	s == vendas n = 1 + (fun s (n - 1))
		|	otherwise = fun s (n - 1)
		
-- Lembrar
[] -- representação de lista vazia
[[], [1 2 3]] -- lista de listas, com a primeira vazia e a segunda com 3 elementos
type String = [Char] -- Uma String é uma lista de char
-- A ordem da lista importa, não é um conjunto
-- Duplicação Ex: [True, True] /= [True]		
-- /= é diferente