(module lang (lib "eopl.ss" "eopl")                

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)


      (expression
       ("showstore")
       showstore-exp)

      (expression (number) const-exp)
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ":" type ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
       ("letrec"
        type identifier "(" identifier ":" type ")" "=" expression
        "in" expression)
       letrec-exp)

      
      ;; new for explicit-refs

      (expression
       ("begin" expression (arbno ";" expression) "end")
       begin-exp)

      (expression
       ("newref" "(" expression ")")
       newref-exp)

      (expression
       ("deref" "(" expression ")")
       deref-exp)

      (expression
       ("setref" "(" expression "," expression ")")
       setref-exp)

      (expression
       ("for" identifier "=" expression "to" expression "(" expression ")")
       for-exp)

      ;; New For Units
      (expression
          ("unitE")
          unit-exp)

      ;; New For pairs
      (expression
          ("pair" "(" expression "," expression ")")
          pair-exp)

      (expression
          ("unpair" "(" identifier "," identifier ")" "=" expression "in" expression)
          unpair-exp)

      ;; Types
      (type
       ("int")
       int-type)
      
      (type
       ("bool")
       bool-type)

      (type
       ("(" type "->" type ")")
       proc-type)

      (type
          ("unit")
          unit-type)

      (type
          ("ref" "(" type ")")
          ref-type)

      (type
          ("<" type "*" type ">")
          pair-type)
      
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  ;;;;;;;;;;;;;;;; type-to-external-form ;;;;;;;;;;;;;;;;

  ;; type-to-external-form : Type -> List
  ;; Page: 243
  (define type-to-external-form
    (lambda (ty)
      (cases type ty
             (int-type () "int")
             (bool-type () "bool")
             (unit-type () "unit")
             (proc-type (arg-type result-type)
                        (list->string (list
                                          (type-to-external-form arg-type)
                                          '- >
                                          (type-to-external-form result-type))))
             (pair-type (typeA typeB)
                        (string-append
                            "<"
                            (type-to-external-form typeA)
                            "*"
                            (type-to-external-form typeB)
                            ">"
                            ))
             (ref-type (arg-type)
                       (string-append
                           "ref" "("
                           (type-to-external-form arg-type)
                           ")"
                           ))

             )
      ))
  
  )
