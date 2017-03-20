#lang racket

(require rackunit rackunit/text-ui "hw1.rkt" "hw2.rkt")

;; Helper functions won't define with rackunit
;(define symbol-upcase (compose string->symbol (compose string-upcase symbol->string)))
;(define (succ val) (+ 1 val))

(run-tests
    (test-suite "hw1"
                #:before (lambda () (display "Homework 1") (newline))
                (test-suite "seven"
                            (test-case "Basic"
                                       (check-equal? (seven -7) 7)))
                (test-suite "set functions"
                            (check-equal? (unionA '(4) '(5)) '(5 4)))
                ))

(run-tests
    (test-suite "hw2"
                #:before (lambda () (display "Homework 2") (newline))
                (test-suite "dTree-height"
                            (test-case "Single Leaf"
                                       (check-equal? (dTree-height (leaf-t 3)) 0))
                            (test-case "tLeft"
                                       (check-equal? (dTree-height tLeft) 2))
                            (test-case "tRight"
                                       (check-equal? (dTree-height tRight) 2))
                            )
                (test-suite "dTree-size"
                            (test-case "Single leaf"
                                       (check-equal? (dTree-size (leaf-t 3)) 1))
                            (test-case "tLeft"
                                       (check-equal? (dTree-size tLeft) 5))
                            )
                (test-suite "dTree-perfect"
                            (test-case "Single leaf"
                                       (check-true (dTree-perfect? (leaf-t 3))))
                            (test-case "tLeft flip"
                                       (check-false (dTree-perfect? (node 'w
                                                                         (leaf-t 8)
                                                                         (node 'x
                                                                               (leaf-t 5)
                                                                               (leaf-t 2))))))
                            (test-case "tLeft"
                                       (check-false (dTree-perfect? tLeft)))
                            (test-case "tRight"
                                       (check-true (dTree-perfect? tRight)))
                            )
                (test-suite "dTree-map"
                            (test-case "Single Leaf"
                                       (check dTree-eq?
                                              (dTree-map symbol-upcase succ (leaf-t 3))
                                              (leaf-t 4)
                                              ))
                            (test-case "tLeft pass"
                                       (check dTree-eq?
                                              (dTree-map symbol-upcase succ tLeft)
                                              (node 'W (node 'X (leaf-t 3) (leaf-t 6)) (leaf-t 9))
                                              ))
                            (test-case "tLeft fail"
                                       (check-false (dTree-eq?
                                              (dTree-map symbol-upcase succ tLeft)
                                              (node 'Z (node 'X (leaf-t 3) (leaf-t 6)) (leaf-t 9))
                                              ))
                                       )
                            )
                (test-suite "list->tree"
                            (test-case "Empty"
                                       (check dTree-eq?
                                              (list->tree '())
                                              (leaf-t 0)
                                              ))
                            (test-case "Length 2"
                                       (check dTree-eq?
                                              (list->tree '(x y))
                                              (node 'x
                                                    (node 'y (leaf-t 0) (leaf-t 0))
                                                    (node 'y (leaf-t 0) (leaf-t 0)))
                                              ))
                            )
                (test-suite "replaceLeafAt and bf->dTree"
                            (test-case "Empty bf"
                                       (check dTree-eq?
                                              (bf->dTree '(() (() . 0)))
                                              (leaf-t 0)
                                              ))
                            (test-case "Empty replace"
                                       (check dTree-eq?
                                              (replaceLeafAt (list->tree '()) '(() (() . 0)))
                                              (leaf-t 0)
                                              ))
                            (test-case "Length 3"
                                       (check dTree-eq?
                                              (bf->dTree xyzGraph)
                                              (replaceLeafAt xyzTree xyzGraph)
                                              ))
                            )
                )
    )
