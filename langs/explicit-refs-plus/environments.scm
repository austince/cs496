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
                    (list 'i 'v 'x)
                    (list (num-val 1) (num-val 5) (num-val 10))
                            (empty-env))
                ))

        ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

        (define apply-env
            (lambda (env search-sym)
;                (write env) (newline) (write search-sym) (newline)
                (cases environment env
                       (empty-env ()
                                  (eopl:error 'apply-env "No binding for ~s" search-sym))

                       (extend-env (bvars bvals saved-env)
                                   (cond
                                        ((location search-sym bvars)
                                             => (lambda (n)
                                                    (list-ref bvals n)))
                                        ; Keep recursing
                                        (else (apply-env saved-env search-sym))))

                       (extend-env-rec* (p-names b-vars p-bodies saved-env)
                                        (cond
                                            ((location search-sym p-names)
                                             => (lambda (n)
                                                    (proc-val
                                                        (procedure
                                                            (list-ref b-vars n)
                                                            (list-ref p-bodies n)
                                                            env))))
                                            ; Keep recursing
                                            (else (apply-env saved-env search-sym)))))))

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