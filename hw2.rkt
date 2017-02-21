#lang racket

(require eopl/eopl)

; dTree
(define-datatype dTree dTree?
  (leaf-t (val number?))
  (node (key symbol?)
      (left dTree?)
        (right dTree?)))

; dTree -> bool
(define (leaf-t? tree)
                (cases dTree tree
                       (leaf-t (val) #t)
                       (else #f)
                       ))

; dTree -> bool
(define (node? tree)
                (cases dTree tree
                       (node (k l r) #t)
                       (else #f)
                       ))

; dTree.leaf-t -> num
(define (leaf-t-val leaf) (cases dTree leaf (leaf-t (val) val) (else (error "Supply a leaf"))))

; dTree.node -> dTree
(define (node-l n) (cases dTree n (node (k l r) l) (else (error "Supply a node"))))

; dTree.node -> dTree
(define (node-r n) (cases dTree n (node (k l r) r) (else (error "Supply a node"))))

; dTree.node -> symbol
(define (node-k n) (cases dTree n (node (k l r) k) (else (error "Supply a node"))))

; dTree -> num
(define dTree-height
    (lambda (tree)
        (cases dTree tree
               (leaf-t (val) 0)
               (node (k l r) (max (+ 1 (dTree-height l)) (+ 1 (dTree-height r)))
               ))
    ))

; dTree -> num
(define dTree-size
    (lambda (tree)
        (cases dTree tree
               (leaf-t (val) 1)
               (node (k l r) (+ 1 (+ (dTree-size l) (dTree-size r))))
               ))
    )

; { dTree, [num] } -> [[num]]
(define dTree-paths-helper
    (lambda (tree path)
        (cases dTree tree
               ;; Make a list out of the path when we hit a leaf
               (leaf-t (val) (list path))
               ;; append all sublists into one bigger list
               (node (k l r) (append (dTree-paths-helper l (cons 0 path))
                                 (dTree-paths-helper r (cons 1 path))))
               ))
    )

; dTree -> [[num]]
(define dTree-paths
    (lambda (tree) (dTree-paths-helper tree '())))

; TODO --> why does this work? luck?
; { dTree, num } -> num|bool
(define dTree-perfect-helper
    (lambda (tree depth)
        (cases dTree tree
               (leaf-t (val) depth)
               (node (k l r) (eq? (dTree-perfect-helper l (+ 1 depth))
                                  (dTree-perfect-helper r (+ 1 depth))))
               ))
    )

; Search in preorder for leaf depths
; dTree -> bool
(define dTree-perfect?
    (lambda (tree)
        (cases dTree tree
               ; base case
               (leaf-t (val) #t)
               (node (k l r) (eq? (dTree-perfect-helper l 0) (dTree-perfect-helper r 0)))
               ))
    )

; { symbol -> symbol, num -> num, dTree } -> dTree
(define dTree-map
    (lambda (sFunc numFunc tree)
        (cases dTree tree
               (leaf-t (val) (leaf-t (numFunc val)))
               (node (k l r) (node (sFunc k)
                                   (dTree-map sFunc numFunc l)
                                   (dTree-map sFunc numFunc r)))
               ))
    )

; { dTree, dTree } -> bool
(define dTree-eq?
    (lambda (t1 t2)
        (cases dTree t1
               (leaf-t (val) (and (leaf-t? t2) (eq? (leaf-t-val t1) (leaf-t-val t2))))
               (node (k l r) (and (node? t2)
                                  (and (eq? (node-k t1) (node-k t2)))
                                      (and (dTree-eq? l (node-l t2)) (dTree-eq? r (node-r t2)))))
               ))
    )

; [symbol] -> dTree
(define list->tree
    (lambda (l)
        (match l
               ['() (leaf-t 0)]
               [(cons h t) (node h (list->tree t) (list->tree t))])
        )
    )

; @type graphEncoding(n) : Pair(List<symbol>(n), List<Pair(List<num>(n), num)>(P(n)))
; where P(n) is the powerset of n

; Strips one from the height of the graph based on the decision
; { graphEncoding(n), num } -> graphEncoding(n-1)
(define (stripGraph graph dec)
    (match graph
           ['() '()] ; Should never get here
           [(cons h t) (cons ; Construct a new graphEncoding pair
                           (cdr h) ; the remaining symbols
                           ;; Could also be accomplished with a fold
                           (map (lambda (x) (cons (cdr (car x)) (cdr x)))
                                        (filter (lambda (x) (eq? (car (car x)) dec)) t))
                           )
           ]
    ))

; { dTree, graphEncoding(n) } -> dTree
;   where dTree is a perfect tree and dTree-height == n - 1
(define replaceLeafAt
    (lambda (t fGraph)
        (cases dTree t
               ; in the form '(() (() . num))
               (leaf-t (val) (leaf-t (cdr (cadr fGraph))))
               (node (k l r) (node k
                                   (replaceLeafAt l (stripGraph fGraph 0))
                                   (replaceLeafAt r (stripGraph fGraph 1))))
               ))
    )

; graphEncoding( -> dTree
(define (bf->dTree fGraph)
        (replaceLeafAt (list->tree (car fGraph)) fGraph))


;; HELPERS FOR TESTING

(define t1
  (node 'r (leaf-t 3) (leaf-t 4)))

(define t1Copy
    (node 'r (leaf-t 3) (leaf-t 4)))

(define t2
  (node 'r
   (node 'x (leaf-t 3) (leaf-t 4))
   (node 'y (leaf-t 5) (leaf-t 6))))

(define tLeft
    (node 'w
        (node 'x (leaf-t 2) (leaf-t 5))
        (leaf-t 8)))

(define tRight
    (node 'w
        (node 'x (leaf-t 5) (leaf-t 5))
        (node 'y (leaf-t 7) (leaf-t 5))))

(define xyzGraph '((x y z) .
   (((0 0 0) . 0)
    ((0 0 1) . 1)
    ((0 1 0) . 1)
    ((0 1 1) . 0)
    ((1 0 0) . 1)
    ((1 0 1) . 0)
    ((1 1 0) . 0)
    ((1 1 1) . 1)
   )))
(define xyzTree (list->tree '(x y z)))

(define symbol-upcase (compose string->symbol (compose string-upcase symbol->string)))

(define (succ val) (+ 1 val))

(provide (all-defined-out))