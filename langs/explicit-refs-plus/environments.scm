(module environments (lib "eopl.ss" "eopl")

        (require "data-structures.scm")
        (provide init-env empty-env extend-env apply-env)

        ;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;


        ;; init-env : () -> Env
        ;; usage: (init-env) = [i=1, v=5, x=10]
        ;; (init-env) builds an environment in which i is bound to the
        ;; expressed value 1, v is bound to the expressed value 5, and x is
        ;; bound to the expressed value 10.
        ;; Page: 69
        (define init-env
            (lambda ()
                (extend-env
                    (list 'i) (list (num-val 1))
                    (extend-env
                        (list 'v) (list (num-val 5))
                        (extend-env
                            (list 'x) (list (num-val 10))
                            (empty-env))))))

        ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

        (define apply-env
            (lambda (env search-syms)
                (cases environment env
                       (empty-env ()
                                  (eopl:error 'apply-env "No binding for ~s" search-syms))

                       (extend-env (bvars bvals saved-env)
                                   (if (equal? search-syms bvars)
                                       bvals
                                       ; Keep recursing
                                       (apply-env saved-env search-syms)))

                       (extend-env-rec* (p-names b-vars p-bodies saved-env)
                                        (cond
                                            ((location search-syms p-names)
                                             => (lambda (n)
                                                    (proc-val
                                                        (procedure
                                                            (list-ref b-vars n)
                                                            (list-ref p-bodies n)
                                                            env))))
                                            ; Keep recursing
                                            (else (apply-env saved-env search-syms)))))))

        ;; location : Listof(Sym) * Listof(Listof(Sym)) -> Maybe(Int)
        ;; (location sym syms) returns the location of sym in syms or #f is
        ;; sym is not in syms.  We can specify this as follows:
        ;; if (memv sym syms)
        ;;   then (list-ref syms (location sym syms)) = sym
        ;;   else (location sym syms) = #f
        (define location
            (lambda (syms allSyms)
                (cond
                    ((null? allSyms) #f)
                    ((equal? syms (car allSyms)) 0)
                    ((location syms (cdr allSyms))
                     => (lambda (n)
                            (+ n 1)))
                    (else #f))))

        )