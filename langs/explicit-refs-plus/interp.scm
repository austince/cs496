(module interp (lib "eopl.ss" "eopl")

        ;; interpreter for the EXPLICIT-REFS language

        (require "drscheme-init.scm")

        (require "lang.scm")
        (require "data-structures.scm")
        (require "environments.scm")
        (require "store.scm")

        (provide value-of-program value-of instrument-let instrument-newref)

        ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

        (define instrument-let (make-parameter #f))

        ;; say (instrument-let #t) to turn instrumentation on.
        ;;     (instrument-let #f) to turn it off again.

        ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

        ;; value-of-program : Program -> ExpVal
        ;; Page: 110
        (define value-of-program
            (lambda (pgm)
                (initialize-store!) ; new for explicit refs.
                (cases program pgm
                       (a-program (exp1)
                                  (value-of exp1 (init-env))))))

        ;; range : Num * Num -> Listof(Num)
        ;; Positive iteration of range
        (define range
            (lambda (start end)
                ;                (eopl:printf "start ~s to end ~s \n" start end)
                (if (>= start end)
                    '()
                    (cons start (range (+ start 1) end))))
            )

        ;; value-of : Exp * Env -> ExpVal
        ;; Page: 113
        (define value-of
            (lambda (exp env)
                (cases expression exp

                       ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
                       (const-exp (num) (num-val num))

                       ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
                       (var-exp (var) ; (write var) (newline)
                                (apply-env env var))

                       ;\commentbox{\diffspec}
                       (diff-exp (exp1 exp2)
                                 (let ((val1 (value-of exp1 env))
                                       (val2 (value-of exp2 env)))
                                     (let ((num1 (expval->num val1))
                                           (num2 (expval->num val2)))
                                         (num-val
                                             (- num1 num2)))))

                       ;\commentbox{\zerotestspec}
                       (zero?-exp (exp1)
                                  (let ((val1 (value-of exp1 env)))
                                      (let ((num1 (expval->num val1)))
                                          (if (zero? num1)
                                              (bool-val #t)
                                              (bool-val #f)))))

                       ;\commentbox{\ma{\theifspec}}
                       (if-exp (exp1 exp2 exp3)
                               (let ((val1 (value-of exp1 env)))
                                   (if (expval->bool val1)
                                       (value-of exp2 env)
                                       (value-of exp3 env))))

                       ;\commentbox{\ma{\theletspecsplit}}
                       (let-exp (var exp body)
                                (let ((val (value-of exp env)))
                                    ;                                    (write var) (newline)
                                    (value-of body
                                              ; Make into lists
                                              (extend-env (list var) (list val) env))))

                       (proc-exp (vars body)
                                 ; No change needed
                                 (proc-val (procedure vars body env)))

                       (call-exp (rator rands)
                                 (let ((proc (expval->proc (value-of rator env)))
                                       ; args is now a list
                                       ;                                       (args (value-of rands env))
                                       (args (map (lambda (rand) (value-of rand env)) rands))
                                      )
                                     (apply-procedure proc args)))

                       (letrec-exp (p-names b-vars p-bodies letrec-body)
                                   (value-of letrec-body
                                             (extend-env-rec* p-names b-vars p-bodies env)))

                       (begin-exp (exp1 exps)
                                  (letrec
                                          ((value-of-begins
                                               (lambda (e1 es)
                                                   (let ((v1 (value-of e1 env)))
                                                       (if (null? es)
                                                           v1
                                                           (value-of-begins (car es) (cdr es)))))))
                                      (value-of-begins exp1 exps)))

                       (newref-exp (exp1)
                                   (let ((v1 (value-of exp1 env)))
                                       (ref-val (newref v1))))

                       (deref-exp (exp1)
                                  (let ((v1 (value-of exp1 env)))
                                      (let ((ref1 (expval->ref v1)))
                                          (deref ref1))))

                       (setref-exp (exp1 exp2)
                                   (let ((ref (expval->ref (value-of exp1 env))))
                                       (let ((v2 (value-of exp2 env)))
                                           (begin
                                               (setref! ref v2)
                                               (num-val 23)))))


                       (for-exp (var expStart expEnd expBody)
                                ;                                (write var) (newline)
                                ;                                (write expStart) (newline)
                                ;                                (write expEnd) (newline)
                                ;                                (write expBody) (newline)
                                (let* (
                                          (startVal (value-of expStart env))
                                          (startNum (expval->num startVal))
                                          (endVal (value-of expEnd env))
                                          (endNum (expval->num endVal))
                                          (rangeList (range startNum endNum))
                                      )
                                    (for-each
                                        ; i is the current iteration value
                                        (lambda (i)
;                                            (begin
;                                                (eopl:printf "iter: ~s\n" i)
                                                (value-of expBody
                                                          (extend-env (list var) (list (num-val i)) env)
                                                          )
;                                                )

                                            )
                                        rangeList
                                        )
                                    )
                                )
                       )))

        ;; apply-procedure : Proc * ExpVal -> ExpVal
        ;;
        ;; uninstrumented version
        ;;   (define apply-procedure
        ;;    (lambda (proc1 arg)
        ;;      (cases proc proc1
        ;;        (procedure (bvar body saved-env)
        ;;          (value-of body (extend-env bvar arg saved-env))))))

        ;; instrumented version
        (define apply-procedure
            (lambda (proc1 arg)
                (cases proc proc1
                       (procedure (var body saved-env)
                                  (let ((r arg))
                                      (let ((new-env (extend-env var r saved-env)))
                                          (when (instrument-let)
                                              (begin
                                                  (eopl:printf
                                                      "entering body of proc ~s with env =~%"
                                                      var)
                                                  (pretty-print (env->list new-env))
                                                  (eopl:printf "store =~%")
                                                  (pretty-print (store->readable (get-store-as-list)))
                                                  (eopl:printf "~%")))
                                          (value-of body new-env)))))))


        ;; store->readable : Listof(List(Ref,Expval))
        ;;                    -> Listof(List(Ref,Something-Readable))
        (define store->readable
            (lambda (l)
                (map
                    (lambda (p)
                        (cons
                            (car p)
                            (expval->printable (cadr p))))
                    l)))

        )
  


  
