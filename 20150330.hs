-- FIBONACCI
pfib :: Int-> Int
pfib n		| n == 0 = 0
	  		| n == 1 = 1
	  		| otherwise = pfib(n-1) + (pfib (n-2))

auxFib :: Int->Int->[Int]
auxFib n k	| n==0 = []
			| ((pfib k) `mod` 2) == 0 && (n==1) = [pfib k]
			| (((pfib k) `mod` 2) == 0) = (pfib k):(auxFib (n-1) (k+1))
			| otherwise = (auxFib n (k+1))

fibpares:: Int->[Int]
fibpares n = (auxFib n 0)
-- MaiorMenor

menorMaior :: Int->Int->Int->(Int,Int)
menorMaior x y z | x<y && x<z && y>z = (x,y)
				 | x<y && x<z && z>x = (x,z)
				 | y<z && x>z = (y,x)
				 | y<z = (y,z)
				 | y<x = (z,y)
				 | otherwise = (z,x)

ordenaTripla :: (Int,Int,Int)->(Int,Int,Int)
ordenaTripla (x, y, z)  | x<y && x<z && y>z = (x,z,y)
						| x<y && x<z && z>x = (x,y,z)
						| y<z && x>z = (y,z,x)
						| y<z = (y,x,z)
						| y<x = (z,x,y)
						| otherwise = (z,y,x)
-- RETA E PONTO
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

getX:: Ponto-> Float
getX (x,y) = x

getY:: Ponto-> Float
getY (x,y) = y

isVertical:: Reta->Bool
isVertical (p1,p2) = (getX p1) == (getX p2)

pontoY:: Float->Reta->Float
pontoY x ((x1,y1),(x2,y2)) = ((y2-y1)*(x-x1))/(x2-x1) + y1


--Compressao de Listas
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]
baseExemplo:: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr.Norrell"), ("Fernando","A Game of Thrones")]

membro :: [Int] -> Int -> Bool
membro xs n = (if [True |a <- xs, a==n] == [] then False else True)

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [l1 |(p1,l1)<-bd, p1 == p]

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos bd l = [p1 |(p1,l1)<-bd, l1 == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = if [True |(p1,l1)<-bd, l1 == l]==[] then False else True

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length [l1 |(p1,l1)<-bd, p1 == p]

emprestar ::  BancoDados -> Pessoa -> Livro -> BancoDados
emprestar  bd p l = if (emprestado  bd l )then bd else (p,l):bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l = [(p1,l1)| (p1,l1) <- bd, not (p1 ==p && l1 ==l) ]

-- BancoDados -> Pessoa -> Livro -> BancoDados

--- QUCK SORT AGAIN?

divide:: [Int]->Int->([Int],[Int])->([Int],[Int])
divide [] n p= p
divide (x:xs) n (me, ma) |  x>n = (divide xs n (me,x:ma))
					   | otherwise = divide xs n ((x:me),ma)

qs :: [Int]->[Int]
qs [] = []
qs p = (qs me )++ head p:(qs ma)
		where (me,ma) = (divide (tail p) (head p) ([],[]))

--Text PRocessing
--getWord :: String -> String
--dropWord :: String -> String
--dropSpace :: String -> String
--type Word = String
--splitWords :: String -> [Word]



















