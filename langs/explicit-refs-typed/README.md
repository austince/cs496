
Append   
```scheme
(run "
let append = proc(dest:list(int)) proc(src:list(int))
letrec list(int) appendInner(srcInner:list(int)) = 
if null?(srcInner)
then dest
else cons(car(srcInner), (appendInner cdr(srcInner)) )
in
(appendInner src)
in ((append cons(3, emptylist int)) cons(4, emptylist int))")
```


With inorder for trees:  
```scheme
(run "
let append = proc(dest:list(int)) proc(src:list(int))
letrec list(int) appInner(srcInner:list(int)) = 
if null?(srcInner)
then dest
else cons(car(srcInner), (appInner cdr(srcInner)) )
in
(appInner src)
in

letrec list(int) inorder(x:tree(int)) =
if nullT?(x)
then emptylist int
else ((append (inorder getLST(x))) cons(getData(x), (inorder getRST(x))))

in 
(inorder node(1 , node(2 , emptytree int , emptytree int ) , emptytree int ) )
")
```