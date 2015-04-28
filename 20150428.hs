a)

foldr:: (t->u->u)->u->[t]->u

(+):: (Num k)=> k->k->k

(.) :: (t ->v) ->(u->t)->u->v

map :: (t->b)->[t]->[b]
--map / compose
([t]->[b]) => (t->v) ----------- t = [t1]/ [b] = 

(.).map = (t->b)->(v->[t])->v->[b]

--sum / (.).map
k     = (v->[t])->v->[b] => 

(+).(.).map :: (Num (v->[t])->v->[b])=> (t->b) ->(v->[t])->v->[b])->(v->[t])->v->[b])

--funçao tipo Num??? Erro de tipo?
--foldr usage
(+).(.).map = (t->u->u)
t = (t->b)
u = (v->[t])->v->[b])

(v->[t])->v->[b])->[(t->b)]->(v->[t])->v->[b])

---?????

b)

foldr:: (t->u->u)->u->[t]->u

(.) :: (t ->v) ->(u->t)->u->v

map :: (t->b)->[t]->[b]

(\x y z -> foldr z x y).map :
(\x y z -> foldr z x y)
x  = u
z = (t->u->u)
y = [t]

(\x y z -> foldr z x y) = (u->[t]->(t->u->u)-> u)

--Compose:
(u->t) = map
u = (x->b)
t = [x]->[b]
(t->v) = (u->[t]->(t->u->u)-> u)
t = u
v = [t]->(t->u->u)-> u

-----
t ==[x]->[b] =>  v = [t]->(t->([x]->[b])->([x]->[b]))->[x]->[b]  -- Os dois lados tem t`s mas são t`s diferentes pa
--------
(u->v):
(x->b)->[t]->(t->([x]->[b])->([x]->[b]))->[x]->[b]

(\x y z -> foldr z x y).map  = (x->b)->[t]->(t->([x]->[b])->([x]->[b]))->[x]->[b]

c)
 map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

 map.((.) (foldr (++) [1,2]))

map :: (a->b)->[a]->[b]
(.) :: (d->e)->(c->d)->c->e
(.) :: (g->h)->(f->g)->f->h
foldr ::(i->j->j)->j->[i]->j
(++) :: [k]->[k]->[k]

(foldr (++) [1,2])  => ([k]->[k]->[k])->[k]->[[k]]->[k]
(foldr (++) [1,2]) = [[k]]->[k]

((.) (foldr (++) [1,2])) => ([[k]]->[k])->(u->[[k]])->u->[k]
((.) (foldr (++) [1,2]))  = (u->[[k]])->u->[k]

c->d = (u->[[k]])->u->[k] = ((.) (foldr (++) [1,2]))
c = (u->[[k]])
d = (u->[k])

d->e = (a->b)->[a]->[b] = map
d = (a->b)
e = [a]->[b]

d = (u->[k]) = (a->b) => a = u && [k] = b
e = [u]->[[k]]

c->e = (u->[[k]])->[u]->[[k]]
 map.((.) (foldr (++) [1,2])) = (u->[[k]])->[u]->[[k]]

d)
(foldr).(.)$(!!)

(!!):: [t]->Int	->t

(.):: (t->v)->(u->t)->u->v

foldr:: (t->u->u)->u->[t]->u

(foldr).(.):
(.) = u1->t1
u1  = (t->v)
t1 = (u->t)->u->v
foldr = t1->v1
t1 = (t->u->u)
v1 = (u->[t]->u)

(u2->t2)->u2->v2 = (t->u->u) => t = (u2->t2) && u2 = u = v2
t1 = ((u2->t2)->u2->u2)
v1 = (u2->[u2->t2]->u2)
u1 = (t2->u2)

(foldr).(.) = (t2->u2)->u2->[u2->t2]->u2 
(t2->u2) = !!
t2 = [t3]
u2 = (Int->t3)

(foldr).(.)$(!!) = (Int->t3)->[(Int->t3)->[t3]]->Int->t3
