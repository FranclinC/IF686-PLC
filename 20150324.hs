-- menorMaior
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
 | a == b && b == c = (a, c)
 | a < b && b < c = (a, c)
 | a < b && b == c = (a, c)
 | a == b && b < c = (a, c)
 | a > b && c > a = (b, c)
 | a > b && a > c && b < c = (b, a)
 | a > b && c < b  = (a, b)
 | a < b && c < b && a > c = (c, b)
 
-- ordenaTripla
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c)
 | a == b && b == c = (a, b, c)
 | a < b && b < c = (a, b, c)
 | a < b && b == c = (a, b, c)
 | a == b && b < c = (a, b, c)
 | a == b && b > c = (c, b, a)
 | a == c && b < c = (b, a, c)
 | a > b && c > a = (b, a, c)
 | a > b && a > c && b < c = (b, c, a)
 | a > b && c < b  = (c, b, a)
 | a < b && c < b && a > c = (c, a, b)

 
 --reta
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)
 
fstCord :: Ponto -> Float
fstCord (a, b) = a
 
scndCord :: Ponto -> Float
scndCord (a, b) = b

isVertical :: Reta -> Bool
isVertical ((x1, y1), (x2, y2))
 | y1 == y2 = True
 | otherwise = False

 
-- y - y1/x - x1 = y2 - y1/x2 - x1 
-- dado um x e uma reta, retorne o y tal que (x, y) pertençam a essa reta 
-- Caso a reta seja vertical, ele retorna infinito
pontoY :: Float -> Reta -> Float
pontoY x ((x1, y1), (x2, y2)) = (((x - x1)*(y2 - y1))/(x2 - x1)) + y1


