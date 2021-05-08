#lang racket

(require "grammar.rkt" brag/support)

;;;;;;;;;;;;;;;;;
;; execute code
;;;;

(define (run code [functions SMOG-standard-functions] [mode '(normal)] [line-num 0] [variables (hash)])
  (let [[r (with-handlers [[scripterror?
                            (λ (e) (report-error code line-num variables e))]
                           [exn:fail? ;; catch all genuine errors
                            (λ (e) (report-error code line-num variables (scripterror 'unhandled (list e))))]
                           [exn:break? ;; exit cleanly on ^C
                            (λ (e) (exit))]]
             (run-line line-num code mode variables functions))]]
    (if (string? r)
        (void) ;; or r if we want to see the final state of the variables
        (apply run (append (list code functions) r)))))

(define (run-line line-num code mode variables functions)
  ;; returns a list of (new-mode new-line-num new-variables)
  (if (or (>= line-num (length code)) (< line-num 0))
      (format "end of program reached: ~a" variables)
      (let [[ast (parse-line (list-ref code line-num) variables)]]
        (cond
          [(equal? ast #f) (if (and (list? (first mode)) (member (last (first mode)) '(skip false)))
                               (list mode (+ 1 line-num) variables) ;; ignore parse errors if we're skipping the line anyway (watch out for "endcae" situations, statement will just steamroll over and probably never error out
                               (raise (scripterror 'syntax '())))]
          [(equal? ast '(line)) (list mode (+ line-num 1) variables)]
          [(equal? (first mode) 'normal)
           (run-ast-normal mode (second ast) line-num variables functions)]
          [(equal? (first mode) 'goto)
           (run-ast-goto mode (second ast) line-num variables functions)]  
          [(equal? (first (first mode)) 'case)
           (run-ast-case mode (second ast) line-num variables functions)]
          [(equal? (first (first mode)) 'if)
           (run-ast-if mode (second ast) line-num variables functions)]
          [else (raise (scripterror 'modetype (list mode)))]))))

(define (run-ast-normal mode ast line-num variables functions)
  (case (first ast)
    [(let-statement) (list mode
                           (+ 1 line-num)
                           (hash-set variables
                                     (format "$~a$" (second ast))
                                     (run-expr (third ast) functions variables)))]
    [(case-start) (list (cons `(case ,(run-expr (second ast) functions variables) false) mode)
                        (+ 1 line-num)
                        variables)]
    [(if-start) (list (cons `(if ,(if (truthy? (run-expr (second ast) functions variables)) 'true 'false)) mode)
                      (+ 1 line-num) variables)]
    [(goto) (list '(goto) ;; we now know nothing about where we are
                  (let [[next-line (run-expr (second ast) functions variables)]]
                    (if (integer? next-line)
                        next-line
                        (raise (scripterror 'goto-non-int (list next-line)))))
                  variables)]
    [else (run-expr ast functions variables) ;; side effects - TODO: may not error on invalid forms?
          (list mode (+ 1 line-num) variables)]))

(define (run-ast-goto mode ast line-num variables functions)
  ;; goto mode is what happens after a goto when we don't know what the local structure is so we don't know whether an endif , for instance, is expected
  (case (first ast)
    [(let-statement) (list mode
                           (+ 1 line-num)
                           (hash-set variables
                                     (format "$~a$" (second ast))
                                     (run-expr (third ast) functions variables)))]
    [(case-start) (list (cons `(case ,(run-expr (second ast) functions variables) false) mode)
                        (+ 1 line-num)
                        variables)]
    [(case-label) (list (cons '(case #f skip) mode) (+ 1 line-num) variables)]
    [(if-start) (list (cons `(if ,(if (truthy? (run-expr (second ast) functions variables)) 'true 'false)) mode)
                      (+ 1 line-num) variables)]
    [(if-else if-else-if) (list (cons '(if skip) mode) (+ 1 line-num) variables)]
    [(goto) (list '(goto) ;; we now know nothing about where we are
                  (run-expr (second ast) functions variables) variables)]
    [(if-end case-end) (list mode (+ 1 line-num) variables)] ;; ignore
    [else (run-expr ast functions variables) ;; side effects - TODO: may not error on invalid forms?
          (list mode (+ 1 line-num) variables)]))

(define (run-ast-case mode ast line-num variables functions)
  (define caseval (second (first mode)))
  (define submode (third (first mode)))
  (case (first ast)
    [(case-start) (case submode ;; need to notice these even when skipping so their endcases don't end the larger statement
                    [(true) (run-ast-normal mode ast line-num variables functions)]
                    [(false skip) (list (cons '(case ignored skip) mode) (+ 1 line-num) variables)])]
    [(case-label)
     (case submode
       [(true) (list (cons `(case ,caseval skip) (rest mode)) (+ 1 line-num) variables)] ;; finished matching section
       [(false) (list (cons `(case ,caseval ,(if (member caseval(map (λ (v) (run-expr v functions variables)) (rest ast)))
                                                 'true 'false))
                            (rest mode))
                      (+ 1 line-num) variables)]
       [(skip) (list mode (+ 1 line-num) variables)])]
    [(case-default)
     (if (equal? submode 'false)
         (list (cons `(case ,caseval true) (rest mode)) (+ 1 line-num) variables)
         (list (cons `(case ,caseval skip) (rest mode)) (+ 1 line-num) variables))]
    [(case-end)
     (list (rest mode) (+ 1 line-num) variables)]
    [else
     (case submode
       [(true) (run-ast-normal mode ast line-num variables functions)]
       [(false skip) (list mode (+ 1 line-num) variables)])]))

(define (run-ast-if mode ast line-num variables functions)
  (define if-result (second (first mode)))
  (case (first ast)
    [(if-start) ;; need to notice these even when skipping so their endifs don't end the main statement
     (if (equal? if-result 'true)
         (run-ast-normal mode ast line-num variables functions)
         (list (cons '(if skip) mode) (+ 1 line-num) variables))]
    [(if-else)
     (case if-result
       [(true) (list (cons '(if skip) (rest mode)) (+ 1 line-num) variables)] ;; only one clause can be true
       [(false) (list (cons '(if true) (rest mode)) (+ 1 line-num) variables)]
       [(skip) (list mode (+ 1 line-num) variables)])]
    [(if-else-if)
     (case if-result
       [(true) (list (cons '(if skip) (rest mode)) (+ 1 line-num) variables)] ;; only one clause can be true
       [(false) (list (cons `(if ,(if (truthy? (run-expr (second ast) functions variables)) 'true 'false))
                                 (rest mode))
                           (+ 1 line-num) variables)]
       [(skip) (list mode (+ 1 line-num) variables)])]
    [(if-end) (list (rest mode) (+ 1 line-num) variables)]
    [else (if (equal? if-result 'true)
              (run-ast-normal mode ast line-num variables functions)
              (list mode (+ 1 line-num) variables))]))

(define (run-expr ast functions variables)
  (case (first ast)
    [(literal) (if (equal? (string-ref (second ast) 0) #\")
                   (trim-ends "\"" (second ast) "\"")
                   (string->number (second ast)))]
    [(funcall) (function-call (second ast) (drop ast 2) functions variables)]
    [(operation) (function-call (third ast) (list (second ast) (fourth ast)) functions variables)]
    [(delayed-variable) (let [[val (hash-ref variables (format "$~a$" (trim-ends "!" (second ast) "!")) #f)]]
                          (if val val (raise (scripterror 'no-such-variable (list (trim-ends "!" (second ast) "!"))))))]
    [(list-literal) (map (λ (arg) (run-expr arg functions variables)) (rest ast))]
    [else (raise (scripterror 'exprtype (list (first ast))))]))

(define (function-call function-name args functions variables) ;; mostly error checking
  (let [[f (hash-ref functions function-name #f)]] ;; check function exists
                 (if f
                     (if (= (length args) (length (scriptfun-args f))) ;; check arity
                         (let* [[arg-argtypes (map (λ (arg) (cons arg ;; this stuff checks argument types
                                                                  (cond [(string? arg) 'string]
                                                                        [(number? arg) 'number]
                                                                        [(list? arg) 'list])))
                                                   (map (λ (a) (run-expr a functions variables)) args))] ;; execute all subexpressions
                                [args (map (λ (a t) (if (or (equal? t 'any) (equal? (cdr a) t))
                                                        (car a)
                                                        (raise (scripterror 'type (list function-name
                                                                                           (scriptfun-args f)
                                                                                           (map cdr arg-argtypes))))))
                                           arg-argtypes (scriptfun-args f))]]
                           (apply (scriptfun-proc f) args)) ;; actual function call
                         (raise (scripterror 'arity (list (if (> (length args) (length (scriptfun-args f)))
                                                              "many" "few")
                                                          function-name
                                                          (length (scriptfun-args f))
                                                          (length args)))))
                     (raise (scripterror 'no-such-function (list function-name))))))

(define (var->string v)
  (cond
    [(string? v) v]
    [(number? v) (number->string v)]
    [(list? v) (if (empty? v)
                   "[]"
                   (string-append (foldl (λ (vi s) (string-append s ", " (if (string? vi)
                                                                             (format "\"~a\"" vi)
                                                                             (var->string vi))))
                                         (format "[~a" (if (string? (first v))
                                                           (format "\"~a\"" (first v))
                                                           (var->string (first v))))
                                         (rest v)) "]"))]
    [else (raise (scripterror 'vartype (list v)))]))

(define (truthy? val)
  (not (equal? val 0)))

;;;;;;;;;;;;;;;;;;;
;; error handling
;;;;

(define (report-error code line-num variables error)
  (display (format "Error on line ~a:\n~a\n~a\n~a"
                   line-num
                   (apply string-append (map (λ (n) (if (and (>= n 0) (< n (length code)))
                                                        (format "~a| ~a~a\n" n (list-ref code n)
                                                                (if (= line-num n) "   << ERROR" ""))
                                                        ""))
                                               (range (- line-num 3) (+ line-num 4))))
                   (apply format (cons (hash-ref error-messages (scripterror-type error)) (scripterror-params error)))
                   (if (regexp-match? #rx"\\$[a-zA-Z_]*\\$" (list-ref code line-num))
                       (format "\nwith variables expanded:\n~a| ~a\n"
                               line-num (expand-variables (list-ref code line-num) variables))
                       "")))
  "encountered an error") ;; string return stops trying to run next line

(define error-messages
  (hash 'unhandled "unhandled exception (this message should not appear). error is:\n ~a"
        'syntax "syntax error"
        'goto-non-int "cannot goto line not indexed by an integer: ~a"
        'no-such-function "no function/operator with the name '~a' exists"
        'no-such-variable "no variable with the name '~a' exists"
        'invalid-argument "invalid argument for function ~a: ~a (~a)"
        'arity "too ~a arguments to function '~a' (expected ~a, got ~a)"
        'type "argument type error calling function/operator '~a': expected types ~a, got ~a"
        'modetype "encountered an unrecognised reading mode (shouldn't happen). value: ~a"
        'vartype "encountered variable of unknown type (shouldn't happen). value: ~a"
        'exprtype "encountered expression of unknown type (shouldn't happen). name: ~a"))

(struct scripterror (type params))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tokenise/parse/expand
;;;;

(define (parse-line line variables)
  (with-handlers [[exn:fail:parsing? (λ (e) #f)]]
    (parse-to-datum (tokenise (expand-variables line variables)))))

(define (expand-variables line variables [n 10])
  ;; expand all $variables$ in a line
  (if (> n 0)
      (let [[match (regexp-match #rx"\\$[a-zA-Z_]*\\$" line)]]
        (if match
            (expand-variables (regexp-replace #rx"\\$[a-zA-Z_]*\\$" line
                                              (regexp-replace-quote (var->string (hash-ref variables (first match) ""))))
                              variables
                              (- n 1))
            line))
      "[variable expansion cut off]"))

(define (tokenise line [running '()])
  (let [[next-token (regexp-cond
                     line
                     ;; non-tokens
                     [#rx"^[ \t]" 'ws]
                     [#rx"^//.*$" 'comment]
                     [#rx"^$" 'eol]
                     ;; keywords
                     [#rx"^let " (token 'LET (first Match))]
                     [#rx"^case " (token 'CASE (first Match))]
                     [#rx"^default" (token 'DEFAULT (first Match))]
                     [#rx"^endcase" (token 'ENDCASE (first Match))]
                     [#rx"^if " (token 'IF (first Match))]
                     [#rx"^elseif " (token 'ELSEIF (first Match))] ;; must come before else
                     [#rx"^else" (token 'ELSE (first Match))]
                     [#rx"^endif" (token 'ENDIF (first Match))]
                     [#rx"^goto " (token 'GOTO (first Match))]
                     ;; generic tokens
                     [#rx"^[a-zA-Z_]+" (token 'NAMESTR (first Match))]
                     [#rx"^![a-zA-Z_]+!" (token 'DELAYEDVARIABLE (first Match))]
                     [#rx"^-?[0-9]+(\\.[0-9]+)?" (token 'NUMBER (first Match))]
                     [#rx"^\"[^\"]*\"" (token 'STRING (first Match))]
                     [#rx"^(==|!=|[-+><?/*~&%|^]+)" (token 'OPERATOR (first Match))] ;; can't tokenise = as an operator because let needs to distinguish it in the grammar
                     [else (list->string (list (string-ref line 0)))])]]
    (cond
      [(or (equal? next-token 'comment)
            (equal? next-token 'eol))
       running]
      [(equal? next-token 'ws)
       (tokenise (substring line 1) running)]
      [else
       (tokenise (substring line (if (token-struct? next-token)
                                     (string-length (token-struct-val next-token))
                                     (string-length next-token)))
                 (append running (list next-token)))])))

(define-syntax (regexp-cond stx)
  (let* [[dtm (syntax->datum stx)]
         [str (cadr dtm)]
         [cases (cddr dtm)]]
    (datum->syntax
     stx
     `(cond ,@(map (λ (case)
                     (if (equal? (car case) 'else)
                         case
                         `[(regexp-match? ,(car case) ,str)
                           (let [[Match (regexp-match ,(car case) ,str)]]
                             ,@(cdr case))])) cases)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; standard functions
;;;;

(struct scriptfun (proc args))

(define SMOG-standard-functions
  (hash ;; functions
   "print" (scriptfun (λ (s) (display (var->string s)) 1) '(any))
   "first" (scriptfun (λ (l) (if (empty? l) (raise (scripterror 'invalid-argument '("first" "[]" "list must not be empty"))) (first l))) '(list))
   "rest" (scriptfun (λ (l) (if (empty? l) (raise (scripterror 'invalid-argument '("rest" "[]" "list must not be empty"))) (rest l))) '(list))
   "contains" (scriptfun (λ (l v) (if (member v l) 1 0)) '(list any))
   "list_add_front" (scriptfun (λ (l v) (cons v l)) '(list any))
   "list_add_back" (scriptfun (λ (l v) (append l (list v))) '(list any))
   "list_remove" (scriptfun (λ (l v) (remove v l)) '(list any))
   "implode" (scriptfun string-join '(list string))
   "string_to_char_list" (scriptfun (λ (s) (map (curry format "~a") (string->list s))) '(string))
   "ascii_code_to_char" (scriptfun (λ (n) (format "~a" (integer->char n))) '(number))
   "char_to_ascii_code" (scriptfun (λ (c) (char->integer (first (string->list c)))) '(string))
   "split_on" (scriptfun string-split '(string string))
   "read" (scriptfun (λ () (let [[line (read-line)]]
                             (if (eof-object? line) "" line))) '())
   "not" (scriptfun (λ (v) (if (truthy? v) 0 1)) '(any))
   
   ;; operations
   "+" (scriptfun + '(number number))
   "-" (scriptfun - '(number number))
   "*" (scriptfun * '(number number))
   "/" (scriptfun / '(number number))
   ">" (scriptfun (λ (a b) (if (> a b) 1 0)) '(number number))
   "<" (scriptfun (λ (a b) (if (< a b) 1 0)) '(number number))
   "%" (scriptfun remainder '(number number))
   "++" (scriptfun string-append '(string string))
   "|" (scriptfun (λ (a b) (if (or (truthy? a) (truthy? b)) 1 0)) '(any any))
   "&" (scriptfun (λ (a b) (if (and (truthy? a) (truthy? b)) 1 0)) '(any any))
   "==" (scriptfun (λ (a b) (if (equal? (var->string a) (var->string b)) 1 0)) '(any any))
   "!=" (scriptfun (λ (a b) (if (equal? (var->string a) (var->string b)) 0 1)) '(any any))))

;;;;;;;;;;;;
;; TESTING
;;;;

#|
(define test-code
  '("let x = \"hello world, how are you?\""
    "if 1 == 1:"
    "  print(\"$x$\n\")"
    "endif"))

(run test-code '(normal) 0 (hash))
|#

;;;;;;;;;;;;
;; exports
;;;;

(provide run SMOG-standard-functions (struct-out scriptfun))
