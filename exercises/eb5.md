

# Exercise 1
```scheme
#lang racket
(define x 2)

> x
2
> (set! x 3)
> x
3
> (set! x 4) x 
4
> (begin (set! x 5) x)
5
> x x 
5
5
> (begin x x)
5
> 
```

# Exercise 2
```scheme
#lang racket
(define x 2)
(define y x)

> (set! x 3)
> y
2
> x
3
> 
```

Define passes by value ?

# Exercise 3
```scheme
#lang racket
(define u '(1 2))
(define v u)

> (set! u '(3))
> u
'(3)
> v
'(1 2)
> 
```
Still passes by value ?

# Exercise 4
```scheme
#lang racket
(define counter
  (let ((local-state 0))
    (lambda ()
      (let ((dummy (set! local-state (+ local-state 1))))
        local-state))))

> (counter)
1
> (counter)
2
> (+ (counter) (counter))
7
> (eq? (counter) (counter))
#f
> 
```
Each reference / call changes the state of the counter

# Exercise 5

```scheme
#lang racket
(define stack
  (let ((stk '()))
    (lambda (message)
      (case message
        ((empty?) (lambda ()
                    (null? stk)))
        ((push!) (lambda (x)
                   (set! stk (cons x stk))))
        ((pop!) (lambda ()
                  (set! stk (cdr stk))))
        ((top) (lambda ()
                 (car stk)))
        (else (error "Stack: Invalid message" message))))))


> ((stack 'push!) 1)
> ((stack 'push!) 2)
> ((stack 'top))
2
> ((stack 'pop!))
> ((stack 'top))
1
> 
```

# Exercise 6

```scheme
#lang racket
(define (ex1 v1 v2)
  (let ((f (let ((combo '()))
    (lambda (x)
      (begin
        (set! combo (cons x combo))
        combo)
         ))))
    (begin
      (f v1)
      (f v2))))

> (ex1 2 1)
'(1 2)
> (ex1 1 2)
'(2 1)
> 
```

# Exercise 7

```scheme
#lang racket
(require compatibility/mlist)
(define c (mcons 1 2))
(define d (mcons 0 c))
(define e (mcons 0 c))


> (set-mcdr! c 5)
> d
(mcons 0 (mcons 1 5))
> e
(mcons 0 (mcons 1 5))
> 
```
Since the cdr of both d and e are c, they are both updated.  
Mutations work by reference.


# Exercise 8

```scheme
#lang racket
(require compatibility/mlist)

> (define l (mcons 'a (mcons 'b '())))
> (set-mcdr! (mcdr l ) l)
> (mcar l)
'a
> (mcar (mcdr (mcdr (mcdr l))))
'b
```
L is a list that is circular and linked to itself. 
a -> b -> a -> b .....

As seen here:  
```scheme 
> (mcar (mcdr (mcdr l)))
'a
> 
```


# Exercise 9

```scheme
#lang racket
(define x 2)
(define (modify y)
  (set! y 5))

> (modify x)
> x
2
> 
```
The other value possible value might have been 5, but here it is clear that
`x` has been passed by value instead of by reference.

# Exercise 10

```scheme
#lang racket
(require compatibility/mlist)
(define x (mcons 1 2))
(define (modify y)
  (set-mcdr! y 5))

> x
(mcons 1 2)
> (modify x)
> x
(mcons 1 5)
> 
```
Mutable pairs are passed by reference, as they can be mutated within closures.