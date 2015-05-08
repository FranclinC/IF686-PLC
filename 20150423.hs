--Respostas dos exercicios que faltaram

-- Quetão do slide 1 lâmina 11

count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs

-- Qual o tipo de [[2,3]]? Lista de lista de Inteiros
--Qual o resultado da avaliação de
--[2,4..9] -> [2, 4, 6, 8]
--[2..2] -> [2]
-- [2, 7..4] -> [2]
--[10,9..1] -> [10,9,8,7,6,5,4,3,2,1]
--[10..1] -> []
--[2,9,8..1] -> Error

--Quicksort slide 1 lâmina 19
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (< x) xs) ++ [x] ++ quicksort (filter (>= x) xs)

-- Fibonnaci, o elemento é a soma dos 2 anteriores, a partir do indice 2: [0, 1, 1, 2, 3, 5.... e assim vai]
fib :: Int -> [Int]
fib 0 = [0]
fib 1 = [1, 0]
fib n = (head (fib (n - 1)) + head (fib (n - 2))) : fib (n - 1)


--Ordenar
order :: [Int] -> [Int]
order [] = []
order (h:t) = order [y | y <- t, (mod y 10 + (div y 10)) < (mod h 10 + (div h 10))] ++ [h] ++ order [y | y <- t, mod y 10 + (div y 10) >= (mod h 10 + (div h 10))]



-- Banco de dados, slide 2 lamina 13
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


-- Quicksort para lista de Inteiros
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (h:t) = quicksort (filter (< h) t) ++ [h] ++ quicksort (filter (>= h) t)


-- Processador de palavras
getWord :: String -> String
getWord [] = []
getWord s = take (getSpace s) s

dropWord :: String -> String
dropWord [] = []
dropWord s = drop (getSpace s) s

dropSpace :: String -> String
dropSpace [] = []
dropSpace (h:t) 
	| x !! 0 /= ' ' = x
	| otherwise = dropSpace t


type Word = String

splitWords :: String -> [Word]
splitWords [] = []
splitWords s
	| (getSpace s == 0 && s!!0 /= ' ') = (getWord s):[]
	| otherwise = (getWord s) : (splitWords $ dropSpace $ dropWord s)


-- Exercicio Slide 3 lamina 6
take :: [t] -> Int -> [t]
take [] _ = []
take (h:t) n
	| (n == 0) = []
	| otherwise = [h] ++ take t (n - 1)

drop :: [t] -> Int -> [t]
drop [] _ = []
drop (h:t) n
	| n == 0 = [h] ++ (drop t n)
	| otherwise = drop t (n - 1)

takeWhile :: (t -> Bool) -> [t] -> [t]
takeWhile f t = [x | x <- t, f x]

dropWhile :: (t -> Bool) -> [t] -> [t]
dropWhile f t = [x | x <- t, not (f x)]


-- SLide 3 lamina 13
qsort :: (Ord t) => [t] -> [t]
qsort [] = []
qsort (h:t) = qsort ([x | x <- t, x < h]) ++ [h] ++ qsort ([x | x <- t, x >= h])


--Slide 3 lamina 14
agrupar::(Eq t)=> [[t]]->[(t,Int)]
agrupar [] = []
agrupar ((a:ys):xs)  = (a,(countLL ((a:ys):xs)) a):(agrupar (removeLL  ((a:ys):xs) a))



--Slide 4 lamina 13
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Show, Eq)

tr:: Tree Int
tr = Node 4 (Node 3 (Node 2 NilT NilT) NilT) (Node 5 NilT NilT)

tl:: Tree Int
tl = Node 3 (Node 4 NilT NilT) (Node 6 NilT NilT)


data List t = Nil | Cons t (List t)
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr:: Expr->String
showExpr (Lit v) = show v
showExpr (Add a b) = showExpr a ++"+"++ showExpr b
showExpr (Sub a b) = showExpr a ++"-"++ showExpr b 

toList:: List t -> [t]
toList Nil = []
toList (Cons t k) = (t:toList(k))

fromList:: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Cons x (fromList xs))

depth:: Tree t -> Int
depth NilT = 0
depth (Node _ a b) = 1+(max (depth a)  (depth b))

collapse::Tree t-> [t]
collapse NilT = []
collapse (Node a l r) = [a] ++(collapse l)++ (collapse r)

bfs:: Tree t ->t-> Bool


-- SLide 05
import Data.Char

mapi :: (t -> u) -> [t] -> [u]
mapi f [] = []
mapi f (a:as) = f a : map f as

mapi2 :: (t -> u) -> [t] -> [u]
mapi2 f [] = []
mapi2 f l = [f k|k<-l]
 
alfa::Char->Int
alfa c | 'a'<=c && 'z'>=c  = ((ord c) - ba) + 1
       | 'A'<=c && 'Z'>=c  = ((ord c) - bA)+ 1
	   |otherwise = -1
		where ba = ord 'a' ; bA = ord 'A'
 
posicaoAlfabeto :: String->[Int]
posicaoAlfabeto s = mapi alfa s  


member::(Eq t)=>t->[t]->Bool
member v ls = foldr (func v) False ls

func::(Eq t)=>t->t->Bool->Bool
func a v b = v==a ||b

union::(Eq t)=>[t]->[t]->[t]
union [] l2 = l2
union (h1:t1) l2 = (foldr (func2) (h1:t1) l2)


func2::(Eq t)=>t->[t]->[t]
func2 v [] = v:[]
func2 v (h2:t2) | v == h2 = v:t2
			    | otherwise = h2:(func2 v t2)


somaElem::[String]->[Int]
somaElem [] = []
somaElem (h1:t1) = (foldr (+) 0(posicaoAlfabeto h1)): somaElem t1

data Tree t = Nil |Tree t (Tree t) (Tree t) deriving (Show, Ord, Eq)

insert::(Ord t) => t->Tree t->Tree t
insert  v Nil = Tree v Nil Nil
insert  v (Tree k a b)| k>v = (Tree k (insert v a) b)
                      | otherwise = (Tree k  a (insert v b))


foldTree:: (t->u->u->u)-> u->Tree t -> u
foldTree _ s NilT = s
foldTree f s (Node a tr1 tr2) = f a (foldTree f s tr1) (foldTree f s tr2) 


filterLists:: [[Int]]->Int->[[Int]]
filterLists l v =  filter (\x->(foldr (+) 0 x)>= v) l

inter::(Eq t)=>[t]->[t]->[t]
inter [] _ = []
inter _ [] = []
inter (x:xs) ys= (filter (==x) ys) ++ (inter xs ys)

diff::(Eq t)=>[t]->[t]->[t]
diff [] y = y
diff x [] = x
diff x y  |  int == [] = x++y
		  |  otherwise = (diff (filter (/=(head int)) x) (filter (/=(head int)) y))
                  where int = (inter y x)


mapfilter::(t->Bool)->[[t]]->[[t]]
mapfilter f [] = []
mapfilter f (x:xs) = [a | a<-x, f a]:(mapfilter f xs )