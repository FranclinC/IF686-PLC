type Key = Int
type Objeto = (Key,String)
type HashTable = [Objeto]

baseTable :: HashTable
baseTable = []

initTable:: HashTable->Int-> HashTable
initTable ht sz | sz == 100  = ht
				| otherwise  = initTable ((-1,""):ht) (sz+1)


hash::Int->Int->Int
hash k m = (k) `mod` m

checkNext :: Int->Int->HashTable->Int
checkNext k v ht| k == (fst (ht!!v)) = v
				| (hash k 100) == v = -1
				| otherwise = checkNext k ((v+1) `mod` 100) ht

--GET
get:: Key->HashTable->Maybe String
get	k ht | k == (fst (ht!!v)) = Just (snd (ht!!v))
		 | (checkNext k (v+1) ht) == -1 = Nothing
		 | otherwise = (Just (snd (ht!!(checkNext k (v+1) ht))))
			where v = ((hash k 100))

nextAvailabe:: Int->Int->Int->HashTable->Int
nextAvailabe k v e h | -1 == (fst (h!!v)) || k == (fst (h!!v))= v
				     | v == e = -1
				     | otherwise  = (nextAvailabe k ((v+1) `mod` 100) e h)

put:: Key->String->HashTable->Maybe HashTable
put k s ht | -1 == (fst (ht!!mk)) || k ==(fst (ht!!mk))  = Just ((take (mk) ht) ++ (k,s):(drop (mk+1) ht))
		   | nb == -1 = Just ht
		   | otherwise =  Just ((take (nb) ht) ++ (k,s):(drop (nb+1) ht))
		 where	mk = ((hash k 100))
		 	nb = (nextAvailabe k (mk+1) mk ht)
--REMOVE
remove::Key->HashTable->Maybe HashTable
remove k ht | k == (fst (ht!!v)) = (Just ((take (v) ht) ++ (-1,""):(drop (v+1) ht)))
			| (checkNext k (v+1) ht) == -1 = Nothing
			| otherwise = (Just ((take (na) ht) ++ (-1,""):(drop (na+1) ht)))
			where v = (hash k 100)
			      na = (checkNext k (v+1) ht)

hasKey:: Key->HashTable->Bool
hasKey k [] = False
hasKey k (x:xs) | k==(fst x) =True
				| otherwise = (hasKey k xs)


--Example
test::Maybe HashTable
test = (put 1 "e1" (initTable baseTable 0))>>=(\x->(put 2 "e2" x))>>= (\x -> remove 2 x)>>=(\x->(put 3 "e3" x))>>=(\x->(put 4 "e4" x))>>= (\x -> remove 4 x)>>=(\x->(put 5 "e5" x))>>=(\x->(put 6 "e6" x))>>= (\x -> remove 6 x)>>=(\x->(put 7 "e7" x))>>=(\x->(put 2 "e2" x))>>= (\x -> remove 2 x)>>=(\x->(put 8 "e8" x))>>=(\x->(put 9 "e9" x))>>= (\x -> remove 9 x)


-------------------------------------------------------
doIt:: IO()
doIt =  do {
		a<-getLine;
		b<-return (process a);
		printi b;
		doIt;
	}
		  --(printi b)
		  --doIt

process::String-> Maybe [String]
process str | check str = (Just (split str))
            | otherwise = Nothing

check:: String->Bool
check []  = True
check (x:xs) | x==' '  || (x>='a' && x<='z') || (x>='A' && x<='Z')  = check xs
		     | otherwise = False

split :: String -> [String]
split [] = []
split x | word /= ""  = (getWord x):(split $ dropSpace $ dropWord $ x)
			 | otherwise = (split $ dropSpace $ dropWord $ x)
	          where word = getWord x

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs) | x/=' ' = dropWord xs
                | otherwise = x:xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace  (x:xs) | x==' ' = dropSpace xs
                  | otherwise = x:xs

getWord :: String -> String
getWord [] = []
getWord (x:xs) | x==' ' =[]
               | otherwise = x:(getWord xs)

printi :: Maybe [String]->IO()
printi Nothing = putStr ""
printi (Just [])  = putStr ""
printi (Just (x:xs)) = do{
				    putStrLn x;
				    printi (Just xs);
                 }