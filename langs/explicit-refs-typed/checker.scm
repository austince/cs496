(module checker (lib "eopl.ss" "eopl")

        (require "drscheme-init.scm")
        (require "lang.scm")

        (provide type-of type-of-program)

        ;; check-equal-type! : Type * Type * Exp -> Unspecified
        ;; Page: 242
        (define check-equal-type!
            (lambda (ty1 ty2 exp)
                (when (not (equal? ty1 ty2))
                    (report-unequal-types ty1 ty2 exp))))

        ;; report-unequal-types : Type * Type * Exp -> Unspecified
        ;; Page: 243
        (define report-unequal-types
            (lambda (ty1 ty2 exp)
                (eopl:error 'check-equal-type!
                            "Types didn't match: ~s != ~a in~%~a"
                            (type-to-external-form ty1)
                            (type-to-external-form ty2)
                            exp)))

        ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;


        ;; Extractors

        ;; Trees
        (define tree-type->type
            (lambda (tree)
                (cases type tree
                       (tree-type (t) t)
                       (else (eopl:error 'tree-type->type
                                         "Not a tree type: ~s" (type-to-external-form tree)))
                       )))

        ;; Pairs

        (define pair-type->fst
            (lambda (pair)
                (cases type pair
                       (pair-type (fstType sndType) fstType)
                       (else (eopl:error 'pair-type->fst
                                         "Not a pair type: ~s" (type-to-external-form pair)))
                       )))

        (define pair-type->snd
            (lambda (pair)
                (cases type pair
                       (pair-type (fstType sndType) sndType)
                       (else (eopl:error 'pair-type->snd
                                         "Not a pair type: ~s" (type-to-external-form pair)))
                       )))

        ;; Lists
        (define list-type->type
            (lambda (lt)
                (cases type lt
                       ;; Make sure it's a list
                       (list-type (lType)
                                  ;; If it is, great!
                                  lType)
                       (else (eopl:error
                                 'null?
                                 "Not a list type: ~s"
                                 (type-to-external-form lt)
                                 )))
                ))

        ;; type-of-program : Program -> Type
        ;; Page: 244
        (define type-of-program
            (lambda (pgm)
                (cases program pgm
                       (a-program (exp1) (type-of exp1 (init-tenv))))))

        ;; type-of : Exp * Tenv -> Type
        ;; Page 244--246
        (define type-of
            (lambda (exp tenv)
                (cases expression exp

                       ;; \commentbox{\hastype{\tenv}{\mv{num}}{\mathtt{int}}}
                       (const-exp (num) (int-type))

                       ;; \commentbox{\hastype{\tenv}{\var{}}{\tenv{}(\var{})}}
                       (var-exp (var) (apply-tenv tenv var))

                       (unit-exp () (unit-type))

                       ;; \commentbox{\diffrule}
                       (diff-exp (exp1 exp2)
                                 (let ((ty1 (type-of exp1 tenv))
                                       (ty2 (type-of exp2 tenv)))
                                     (check-equal-type! ty1 (int-type) exp1)
                                     (check-equal-type! ty2 (int-type) exp2)
                                     (int-type)))

                       ;; \commentbox{\zerorule}
                       (zero?-exp (exp1)
                                  (let ((ty1 (type-of exp1 tenv)))
                                      (check-equal-type! ty1 (int-type) exp1)
                                      (bool-type)))

                       ;; \commentbox{\condrule}
                       (if-exp (exp1 exp2 exp3)
                               (let ((ty1 (type-of exp1 tenv))
                                     (ty2 (type-of exp2 tenv))
                                     (ty3 (type-of exp3 tenv)))
                                   (check-equal-type! ty1 (bool-type) exp1)
                                   (check-equal-type! ty2 ty3 exp)
                                   ty2))

                       ;; \commentbox{\letrule}
                       (let-exp (var exp1 body)
                                (let ((exp1-type (type-of exp1 tenv)))
                                    (type-of body
                                             (extend-tenv var exp1-type tenv))))

                       ;; \commentbox{\procrulechurch}
                       (proc-exp (var var-type body)
                                 (let ((result-type
                                           (type-of body
                                                    (extend-tenv var var-type tenv))))
                                     (proc-type var-type result-type)))

                       ;; \commentbox{\apprule}
                       (call-exp (rator rand)
                                 (let ((rator-type (type-of rator tenv))
                                       (rand-type (type-of rand tenv)))
                                     (cases type rator-type
                                            (proc-type (arg-type result-type)
                                                       (begin
                                                           (check-equal-type! arg-type rand-type rand)
                                                           result-type))
                                            (else
                                                (report-rator-not-a-proc-type rator-type rator)))))

                       ;; \commentbox{\letrecrule}
                       (letrec-exp (p-result-type p-name b-var b-var-type p-body
                                                  letrec-body)
                                   (let ((tenv-for-letrec-body
                                             (extend-tenv p-name
                                                          (proc-type b-var-type p-result-type)
                                                          tenv)))
                                       (let ((p-body-type
                                                 (type-of p-body
                                                          (extend-tenv b-var b-var-type
                                                                       tenv-for-letrec-body))))
                                           (check-equal-type!
                                               p-body-type p-result-type p-body)
                                           (type-of letrec-body tenv-for-letrec-body))))

                       (showstore-exp ()
                                      (eopl:error "not implemented!"))

                       (begin-exp (e exps) (eopl:error "not implemented!"))

                       (for-exp (id lb up body) (eopl:error "not implemented!"))

                       ;; REFERENCES

                       (newref-exp (e)
                                   (let ((eType (type-of e tenv)))
                                       (ref-type eType)))

                       (deref-exp (e)
                                  (let ((eType (type-of e tenv)))
                                      (cases type eType
                                             (ref-type (t) t)
                                             (else (eopl:error
                                                       'deref
                                                       "Not a ref type: ~s in ~a"
                                                       (type-to-external-form eType)
                                                       e
                                                       )))))

                       (setref-exp (le re)
                                   (let ((leType (type-of le tenv))
                                         (reType (type-of re tenv)))
                                       (cases type leType
                                              ;; Check to make sure the value being set and what it's
                                              ;; being set to are of the same type
                                              (ref-type (t)
                                                        (check-equal-type! t reType re))
                                              (else (eopl:error
                                                        'deref
                                                        "Not a ref type: ~s in ~a"
                                                        (type-to-external-form leType)
                                                        le
                                                        )))
                                       (unit-type)))

                       ;; Pairs

                       (pair-exp (le re)
                                 (let ((leType (type-of le tenv))
                                       (reType (type-of re tenv)))
                                     (pair-type leType reType)))

                       (unpair-exp (id1 id2 exp body)
                                   (let* ((expType (type-of exp tenv))
                                          (fstType (pair-type->fst expType))
                                          (sndType (pair-type->snd expType))
                                         )
                                       ;; Extend env with both vars
                                       (type-of body (extend-tenv id1 fstType
                                                                   (extend-tenv id2 sndType tenv)))
                                       ))

                       ;; Lists
                       (emptylist-exp (t) (list-type t))

                       (cons-exp (exp1 exp2)
                                 (let* ((e1Type (type-of exp1 tenv))
                                       (e2Type (type-of exp2 tenv))
                                       (lType (list-type->type e2Type))
                                      )
                                     (check-equal-type! lType e1Type exp1)
                                     ;; Make sure the second arg is a list
                                     ;; The result will be of the second expressions type
                                     e2Type))

                       (null?-exp (exp1)
                                  (let* ((e1Type (type-of exp1 tenv))
                                         (lType (list-type->type e1Type)))
                                      ;; If it's a list, great!
                                      (bool-type)
                                      ))

                       (car-exp (exp1)
                                (let* ((e1Type (type-of exp1 tenv))
                                       (lType (list-type->type e1Type)))
                                    lType
                                    ))

                       (cdr-exp (exp1)
                                (let* ((e1Type (type-of exp1 tenv))
                                      (lType (list-type->type e1Type))
                                     )
                                    e1Type))

                       ;; TREES
                       ;; t -> tree(t)
                       (emptytree-exp (t) (tree-type t))

                       ;; exp(t) * exp(tree(t)) * exp(tree(t)) -> tree(t)
                       (node-exp (valexp lstexp rstexp)
                                 (let* ((valType (type-of valexp tenv))
                                       (lstTree (type-of lstexp tenv))
                                       (rstTree (type-of rstexp tenv))
                                       (rType (tree-type->type rstTree))
                                        (lType (tree-type->type lstTree))
                                      )
                                     (check-equal-type! lType valType lstexp)
                                     (check-equal-type! rType valType rstexp)
                                     ;; The result will be a tree of the valexp type
                                     (tree-type valType)))

                       ;; tree(t) -> bool
                       (nullT?-exp (exp)
                                   (let* ((expType (type-of exp tenv))
                                          (tType (tree-type->type expType))
                                         )
                                       ;; if it's a tree, return bool, doi
                                       (bool-type)
                                       ))

                       ;; tree(t) -> t
                       (getData-exp (exp)
                                    (let* ((expType (type-of exp tenv))
                                           (tType (tree-type->type expType)))
                                        ;; if it's a tree, return the tree's type
                                        tType
                                        ))

                       ;; tree(t) -> tree(t)
                       (getLST-exp (exp)
                                   (let* ((expType (type-of exp tenv))
                                          (tType (tree-type->type expType)))
                                       ;; if it's a tree, return the tree
                                       expType
                                       ))

                       ;; Same as getLST
                       ;; tree(t) -> tree(t)
                       (getRST-exp (exp)
                                   (let* ((expType (type-of exp tenv))
                                          (tType (tree-type->type expType)))
                                       ;; if it's a tree, return the tree
                                       expType
                                       ))
                       )))

        (define report-rator-not-a-proc-type
            (lambda (rator-type rator)
                (eopl:error 'type-of-expression
                            "Rator not a proc type:~%~s~%had rator type ~s"
                            rator
                            (type-to-external-form rator-type))))

        ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

        (define-datatype type-environment type-environment?
                         (empty-tenv-record)
                         (extended-tenv-record
                             (sym symbol?)
                             (type type?)
                             (tenv type-environment?)))

        (define empty-tenv empty-tenv-record)
        (define extend-tenv extended-tenv-record)

        (define apply-tenv
            (lambda (tenv sym)
                (cases type-environment tenv
                       (empty-tenv-record ()
                                          (eopl:error 'apply-tenv "Unbound variable ~s" sym))
                       (extended-tenv-record (sym1 val1 old-env)
                                             (if (eqv? sym sym1)
                                                 val1
                                                 (apply-tenv old-env sym))))))

        (define init-tenv
            (lambda ()
                (extend-tenv 'x (int-type)
                             (extend-tenv 'v (int-type)
                                          (extend-tenv 'i (int-type)
                                                       (empty-tenv))))))

        )
