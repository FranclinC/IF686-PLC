--sqrRoot :: [Int] -> [Int]
--sqrRoot [] = []
--sqrRoot a =  map pot  

pot :: Float -> Float -> Float --eleva 'x' a potencia de y
pot x 0 = 1
pot x y = x * (pot x (y - 1))

--Função que retorna a raíz quadrada
-- Essa função está retornando valor errado, corrigir
sqrRoot :: Float -> Float
sqrRoot x = (pot x 1/2)