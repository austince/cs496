#lang racket
#|
austin cawley-edwards
I pledge my honor that I have abided by the stevens honor system.
|#


#| EX 1
|#

;; a -> num
(define (seven x) 7)

;; num -> num
(define (sign x)
  (if (< x 0) -1 1))

;; num -> num
(define (absolute x)
  (if (< x 0) (* -1 x) x))

;; { bool, bool } -> bool
(define (andp x y)
  (if x (if y #t #f) #f))

;; { bool, bool } -> bool
(define (orp x y)
  (if x #t (if y #t #f)))

;; { bool, bool } -> bool
;; Not should only take one argument, no?
(define (notp x)
  (if x #f #t))

;; { bool, bool } -> bool
(define (xorp x y)
  (if x (if y #f #t) (if y #t #f)))
  
;; { num, num } -> bool
(define (dividesBy x y)
  (eq? (remainder x y) 0))

;; using match
;; [a] -> bool
(define (singleton? xs)
  (match xs
    ['() #f]
    [(cons h xs) (eq? xs '())]
    )
  )

;; using cons? and null?
;; [a] -> bool
(define (singleton2? xs)
  (if (cons? xs) (null? (cdr xs)) #f))

;; pair -> pair
(define (swap p)
  (cons (cdr p) (car p)))

;; { a -> b, a } -> b
(define (app proc x)
  (proc x))

;; { a -> a, a } -> a
(define (twice proc x)
  (proc (proc x)))

;; { b -> c, a -> b, a } -> d
(define (compose procOuter procInner x)
  (procOuter (procInner x)))

#| EX 2
1. intersection, union, belongsTo
|#

;; { a, [a] } -> bool
(define (belongsToA x xs)
  (match xs
    ['() #f]
    [(cons h t) (if (eq? h x) #t (belongsToA x t))]
    )
  )

;; each element must be in xs and ys
;; { [a], [a] } -> [a]
(define (intersectionA xs ys)
  (match xs
    ['() '()]
    [(cons h t) (if (belongsToA h ys)
                    (cons h (intersectionA (cdr xs) ys))
                    (intersectionA (cdr xs) ys))
                    ]
  ))

;; { [a], [a] } -> [a]
(define (unionAHelper xs unionList)
  (match xs
    ['() unionList]
    [(cons h t) (if (belongsToA h unionList)
                    (unionAHelper (cdr xs) unionList)
                    (cons h (unionAHelper (cdr xs) unionList)))
                ]))

;; each element must be in either xs or ys
;; { [a], [a] } -> [a]
(define (unionA xs ys)
  (unionAHelper ys (unionAHelper xs '())))

;; { a, a -> bool } -> bool
(define (belongsToB x charFunc)
  (charFunc x))

;; { a -> bool, a -> bool } -> { a -> bool }
(define (unionB charFunc1 charFunc2)
  (lambda (x) (or (charFunc1 x) (charFunc2 x))))

;; { a -> bool, a -> bool } -> { a -> bool }
(define (intersectionB charFunc1 charFunc2)
  (lambda (x) (and (charFunc1 x) (charFunc2 x))))

;; [a] -> [a]
(define (remDups xs)
  (match xs
    ['() '()]
    [(list x) (list x)]
    ;; if the head is the different from the first element of tail, recursively append,
    ;;  else ignore and keep recursing
    [(cons h t) (if (eq? h (car t)) (remDups t) (append (list h) (remDups t))) ]
    )
  )

;; { a, [[a]] } -> [[a]]
(define (sublistHelper x xs)
  (match xs
    ['() '()]
    [(cons h t)
     ; Use it
     (cons (cons x h)
     ; Lose it
           (cons h (sublistHelper x t)))
     ]))

;; [a] -> [[a]]
(define (sublist xs)
  (match xs
    ['() '(())]
    [(cons h t)
     ; Cycle through
     (sublistHelper h (sublist t))
     ]))

#| EX 3
Calculator functions
|#

;; E is the calulator expression type
;; { num -> num, E } -> E
(define (mapC func exp)
  (match exp
    ['() '()]
    [(list 'const x) (list 'const (func x))]
    [(list op e1 e2) (list op (mapC func e1) (mapC func e2))]))


; { {num} -> a, {E, E} -> a, {E, E} -> a, {E, E} -> a, {E, E} -> a, E } -> a
(define (foldC constF addF subF multF divF exp)
  (let ((recF (lambda (x) (foldC constF addF subF multF divF x))))
  (match exp
    [(list 'const x) (constF x)]
    [(list 'add x y) (addF (recF x) (recF y))]
    [(list 'sub x y) (subF (recF x) (recF y))]
    [(list 'mult x y) (multF (recF x) (recF y))]
    [(list 'div x y) (divF (recF x) (recF y))]
  )))
    
; E -> num
(define (numAdd exp)
  (let ((nop (lambda (e1 e2) 0)))
    (foldC (lambda (x) 0)
           (lambda (x y) (+ 1 x y))
           nop ;; Adds nothing
           nop
           nop
           exp)))

; E -> E
(define (repAddWithMult exp)
  (foldC (lambda (x) (list 'const x))
         (lambda (x y) (list 'mult x y)) ;;add
         (lambda (x y) (list 'sub x y)) ;; sub
         (lambda (x y) (list 'mult x y)) ;; mult
         (lambda (x y) (list 'div x y)) ;; div
         exp))

; E -> num
(define (evalCHelp exp)
  (match exp
    [(list 'const x) x]
    [(list 'add x y) (+ (evalC x) (evalC y))]
    [(list 'sub x y) (- (evalC x) (evalC y))]
    [(list 'mult x y) (* (evalC x) (evalC y))]
    [(list 'div x y) (/ (evalC x) (evalC y))]
  ))

; E -> E
(define (evalC exp) (list 'const (evalCHelp exp)))

; E -> num
(define (evalCFHelp exp)
  (foldC (lambda (x) x)
         (lambda (x y) (+ x y)) ;;add
         (lambda (x y) (- x y)) ;; sub
         (lambda (x y) (* x y)) ;; mult
         (lambda (x y) (/ x y)) ;; div
         exp))

; E -> E
(define (evalCF exp) (list 'const (evalCFHelp exp)))

#| EX 4

|#

; Takes a list of numbers, counts the number of evens
;  using a right fold and a lambda function named 'g'
; [num] -> num
(define (f xs)
  (let ((g (lambda (x r) (if (even? x) (+ r 1) r))))
    (foldr g 0 xs)))


; Does not work oops lol nvrmnd sry
;; Confused on how to use this
;; ((concat '((1 2) (3 4))) or ((concat '((1 2)) '(3 4)) or ((concat '((1 2) (3 4)) '()) ?
(define (concat xss)
  (let ((g (lambda (xs id)
             (lambda (l) (if (null? l)
                             '()
                             (cons xs (id l)))))))
    (foldr g identity xss)))

; [a] -> [a] -> [a]
(define (append2 xs)
  (let ((g (lambda (x id)
             (lambda (l) (if (null? l)
                             (if (null? xs) '() xs)
                             (cons x (id l)))))))
    (foldr g identity xs)))

; For test suite
(provide (all-defined-out))
