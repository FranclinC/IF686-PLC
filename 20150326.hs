pegar:: Int->[t]->[t]
pegar n (a:x) | n==0 = []
			 | otherwise = a:(pegar (n-1) x)

retirar:: Int->[t]->[t]
retirar n [] = []
retirar n (a:x) | n==0 = (a:(retirar 0 x))
			 | otherwise = (retirar (n-1) x)

takeWhile1:: (t->Bool)->[t]->[t]
takeWhile1 func [] = []
takeWhile1 func (a:l1) 	| (func a)  = a:(takeWhile1 func l1)
						| otherwise = []

------------

divide:: (Ord t)=>[t]->t->([t],[t])->([t],[t])
divide [] n p= p
divide (x:xs) n (me, ma)	|  x>n = (divide xs n (me,x:ma))
					 		| otherwise = divide xs n ((x:me),ma)

qs :: (Ord t)=>[t]->[t]
qs [] = []
qs p = (qs me )++ (head p):(qs ma)
		where (me,ma) = (divide (tail p) (head p) ([],[]))

-----------

countLL::(Eq t) =>[[t]]->t->Int
countLL [] e = 0
countLL (a:l) e = (countLL l e) + (countL a e)

countL:: (Eq t)=>[t]->t->Int
countL [] e = 0
countL (a:l) e | a==e = 1 + (countL l e)
			 | otherwise = (countL l e)

removeLL::(Eq t) =>[[t]]->t->[[t]]
removeLL [] e = []
removeLL (a:l) e | (removeL a e) ==[] = (removeLL l e)
				 | otherwise = (removeL a e):(removeLL l e)

removeL:: (Eq t)=>[t]->t->[t]
removeL [] e = []
removeL (a:l) e | a==e = (removeL l e)
			 	| otherwise = a:(removeL l e)


agrupar::(Eq t)=> [[t]]->[(t,Int)]
agrupar [] = []
agrupar ((a:ys):xs)  = (a,(countLL ((a:ys):xs)) a):(agrupar (removeLL  ((a:ys):xs) a))