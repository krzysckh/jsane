(import
 (owl toplevel)
 (owl args)
 (owl sexp)
 (prefix (owl sys) sys/))

(define infix '(+ - / // * ** % = == === != !== && || < > <= >= ^ |\|| << >>))

(define fn-rewrite-alist
  '((and    . &&)
    (or     . ||)
    (not    . !)
    (bnot   . ~)
    (bior   . |\||)
    (ior    . |\||)
    (bxor   . ^)
    (xor    . ^)
    (modulo . %)
    (mod    . %)
    (=      . ==)
    (eq?    . ===)
    (eqv?   . ==)
    (equal? . ==)
    (=      . ==)
    ))

(define (error . args)
  (print-to stderr "error: " (fold str "" args))
  (halt 42))

;; this is funny -vvvvv- i stole this from owl
(define (get-code codes env)
  (let ((get-exp (get env 'get-exp 'bad)))
    (let loop ((env env) (codes codes) (acc #n))
      (if (null? codes)
          (values acc env)
          (lets ((d env (get-exp (car codes) env)))
            (loop env (cdr codes) (append acc (list d))))))))

(define (get-lambda v env)
  (let ((get-exp (get env 'get-exp 'bad))
        (names (get env 'names '())))
    (when (or
           (< (length v) 3)
           (not (list? (cadr v))))
      (error "bad lambda: " v))
    (lets ((codes _ (get-code (cddr v) (put env 'names (append names (cadr v))))))
      (values
       (tuple 'λ (cadr v) codes)
       env))))

(define (get-macro v env)
  (lets ((get-exp (get env 'get-exp 'bad))
         (macro (get (get env 'macros empty) (car v) #f))
         (args (cdr v))
         (m-args m-code (values (ref macro 1) (ref macro 2))))
    (let ((code
           `(let (,@(let loop ((args args) (m-args m-args))
                      (cond
                       ((and (null? m-args) (not (null? args)))
                        (error "args mismatched for macro " macro))
                       ((null? m-args)
                        #n)
                       ((pair? m-args)
                        (append
                         `((,(car m-args) ',(car args)))
                         (loop (cdr args) (cdr m-args))))
                       (else ; (a b . c) <- we are at c
                        `((,m-args '(,@args)))))))
              ,@m-code)))
      ;; (print "code: " code)
      ;; (print "evald: " (eval code *toplevel*))
      (get-exp (eval code *toplevel*) env))))

(define (get-funcall v env)
  (lets ((get-exp (get env 'get-exp 'bad))
         (maybe-macro (get (get env 'macros empty) (car v) #f)))
    (cond
     (maybe-macro (get-macro v env))
     ((has? infix (car v))
      (if (not (= (len (cdr v)) 2))
          (error "invalid number of args for infix!: " v)
          (lets ((e1 _ (get-exp (car (cdr v)) env))
                 (e2 _ (get-exp (cadr (cdr v)) env)))
            (values (tuple 'infix! (car v) e1 e2) env))))
     (else
      (lets ((func _ (get-exp (car v) env))
             (args _ (get-code (cdr v) env)))
        (values
         (tuple 'funcall func args)
         env))))))

(define (get-define v env)
  (lets ((get-exp (get env 'get-exp 'bad))
         (env (put env 'names (append (get env 'names '()) (list (cadr v))))))
    (if (list? (cadr v))
        (get-define
         `(define ,(caadr v) (λ (,@(cdadr v)) ,@(cddr v)))
         env)
        (lets ((code _ (get-code (cddr v) env)))
          (values
           (tuple 'define (cadr v) code)
           env)))))

(define (get-extern v env)
  (lets ((env (put env 'names (append (get env 'names '()) (cdr v)))))
    (values
     (tuple 'noop 'extern)
     env)))

(define (get-defmacro v env)
  (lets ((macros (get env 'macros 'bad))
         (name args code (values
                          (car (cadr v))
                          (cdr (cadr v))
                          (cddr v)))
         (macros (put macros name (tuple args code))))
    (values
     (tuple 'noop 'define-macro)
     (put env 'macros macros))))

(define (get-raw v env)
  (let ((get-exp (get env 'get-exp 'bad)))
    (values
     (tuple '_raw (map (λ (e) (lets ((a _ (get-exp e env))) a)) (cdr v)))
     env)))

(define (get-set! v env)
  (lets ((get-exp (get env 'get-exp 'bad))
         (a _ (get-exp (cadr v) env))
         (b _ (get-exp (caddr v) env)))
    (values
     (tuple 'set! a b)
     env)))

(define (get-funcall-alike v env)
  (let ((get-exp (get env 'get-exp 'bad)))
    (if (null? v)
        (values (tuple 'noop 'huh) env)
        (case (car v)
          ('quote (get-exp (str (cadr v)) env))
          ('λ (get-lambda v env))
          ('define (get-define v env))
          ('define-macro (get-defmacro v env))
          ('extern (get-extern v env))
          ('_raw (get-raw v env))
          ('set! (get-set! v env))
          ;; ('if (get-if v env))
          (else
           (get-funcall v env))))))

(define (object-name ob)
  (string->symbol (car* ((string->regex "c/\\./") (str ob)))))

(define (get-exp v env)
  (cond
   ((list? v)
    (get-funcall-alike v env))
   (else
    (let ((names (get env 'names '())))
      (if-lets ((_ (symbol? v))
                (ob-name (object-name v))
                (_ (and (symbol? v) (not (has? names ob-name)))))
        (error "undeclared variable: " v " - if this is intentional, declare it as (extern " ob-name ")"))
      (values
       (tuple 'value v)
       env)))))

(define jsize-rename-alist
  '((#\- . "_")
    (#\= . "Eql")
    (#\* . "Star")
    (#\! . "Bang")
    (#\@ . "At")
    (#\# . "Hash")
    (#\% . "Perc")
    (#\^ . "Wedgie")
    (#\& . "And")
    (#\? . "_p")
    (#\< . "ALeft")
    (#\> . "ARight")))

(define (jsize-symbol s)
  (if (has? (map cdr fn-rewrite-alist) s)
      s
      (fold
       (λ (a b)
         (if-lets ((v (assoc b jsize-rename-alist)))
           (str a (cdr v))
           (str a (string b))))
       ""
       (string->runes (str s)))))

(define *core*
  (append
   (map
    (λ (f)
      (list 'define-macro (ilist (car f) 'args)
            (append '(quasiquote)
                    (list (list (cdr f) '(unquote-splicing args))))))
    fn-rewrite-alist)

   `((extern console document window Array parseInt String JSON)
     (extern ! ~) ; hah!

     (define-macro (begin . body)
       `((λ () ,@body)))

     (define-macro (let lst . body)
       (if (null? lst)
           `(begin ,@body)
           `((λ (,(caar lst)) (let ,(cdr lst) ,@body)) ,(cadar lst))))

     (define-macro (new type . args)
       `((_raw "new " ,type) ,@args))

     ;; (object (a => "b") (c => "d")) -> {a: "b", c: "d"}
     (define-macro (object . args)
       `(_raw "{" ,@(map
                     (λ (e)
                       `(_raw "\"" ,(car e) "\"" ": " ,(caddr e) ","))
                     args)
              "}"))

     ;; TODO: letrec
     (define-macro (while test . body)
       `(begin
          (define (__f)
            (if ,test (begin ,@body (__f)) #t))
          (__f)))

     (define-macro (spread thing)
       `(_raw "[..." ,thing "]"))

     (define-macro (spreading thing)
       `(_raw "..." ,thing))

     (define-macro (if test then else)
       `(_raw "(" (begin ,test) "?" (begin ,then) ":" (begin ,else) ")"))

     (define-macro (when test then)
       `(if ,test ,then 'undefined))

     (define-macro (list-ref l n)
       `(_raw ,l "[" ,n "]"))

     (define-macro (ref ob key)
       `(_raw ,ob "." ,key))

     (define-macro (car lst)
       `(list-ref ,lst 0))

     (define-macro (cdr lst)
       `((ref ,lst 'slice) 1))

     (define-macro (length ob)
       `(ref ,ob 'length))

     (define-macro (len ob)
       `(length ,ob))

     (define-macro (null? ob)
       `(eq? (length ,ob) 0))

     (define-macro (map f lst)
       `((ref ,lst 'map) ,f))

     (define-macro (take l n)
       `((ref ,l 'slice) 0 ,n))

     (define-macro (drop l n)
       `((ref ,l 'slice) ,n))

     (define-macro (list . args)
       `(Array ,@args))

     (define-macro (element! type)
       `(document.createElement ,type))

     (define-macro (print l)
       `(console.log ,l))

     )))

(define (compile filename)
  (if-lets ((lst (if (string? filename) (file->list filename) (force-ll (port->byte-stream filename))))
            (sexps (list->sexps lst (λ _ _) "syntax error:")))
    (lets ((code _ (get-code
                    (append
                     *core*
                     sexps)
                    (pipe empty
                      (put 'get-exp get-exp)
                      (put 'macros empty)
                      (put 'names '())
                      ))))
      code)))

(define (commize vs)
  (cond
   ((null? vs) "")
   ((null? (cdr vs)) (car vs))
   (else
    (fold
     (λ (a b) (str a "," b))
     (car vs)
     (cdr vs)))))

(define (codegen v opt*)
  (let ((opt (put opt* 'rawstring #f))) ; use rawstring on depth 1 from _raw only
    ;; (print "raw: " (get opt* 'rawstring #f) " for " v)
    (if (list? v)
        (cond
         ((= (len v) 0) "") ; umm
         ((= (len v) 1) (str "return " (codegen (car v) opt) ";"))
         (else
          (str (codegen (car v) opt) "; " (codegen (cdr v) opt))))
        (tuple-case v
          ((define name code)
           (if (not (= (len code) 1))
               (error "invalid code for define: " code)
               (str "const " (jsize-symbol name) " = " (codegen (car code) opt))))
          ((λ args code)
           (str "((" (commize (map jsize-symbol args)) ") => {" (codegen code opt) "})"))
          ((funcall function args)
           (str "(" (codegen function opt) "(" (commize (map (C codegen opt) args)) "))"))
          ((value v)
           (cond
            ((eq? #t v) "true")
            ((eq? #f v) "false")
            ((string? v) (if (get opt* 'rawstring #f)
                             (str v)
                             (str* v)))
            ((symbol? v) (jsize-symbol v))
            (else
             (str v))))
          ((noop _)
           "")
          ((infix! f a b)
           (str "(" (codegen a opt) f (codegen b opt) ")"))
          ((_raw vs)
           (fold str "" (map (C codegen (put opt 'rawstring #t)) vs)))
          ((set! a b)
           (str "(" (codegen a opt) "=" (codegen b opt) ")"))
          (else
           (error "not implemented " v)
           )))))

(define (fully-codegen code)
  (fold (λ (a b)
          (let ((v (codegen b empty)))
            (if (string=? v "")
                a
                (string-append a ";\n" (codegen b empty)))))
        "" code))

(define scheme? (string->regex "m/\\.scm$/"))
(define scheme-ext->js-ext (string->regex "s/\\.scm$/.js/"))

(define command-line-rules
  (cl-rules
   `((help   "-h" "--help")
     (input  "-i" "--input"  has-arg comment "input file directory (not recursive)")
     (output "-o" "--output" has-arg comment "target directory for js files" default "public")
     ;; (ast    "-p" "--print-ast" comment "print the AST")
     )))

(define (full-path d s)
  (format #f "~a/~a" d s))

(define eval-file (B string->bytes (B fully-codegen compile)))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print-rules command-line-rules)
       (halt 0))

     (let ((in (get opt 'input #f))
           (out (get opt 'output #f))
           (ast (get opt 'ast #f)))
       (cond
        ((= (length extra) 1)         (write-bytes stdout (eval-file (car extra))) 0)
        ((and (null? extra) (not in)) (write-bytes stdout (eval-file stdin)) 0)
        ((> (length extra) 1)
         (print-to stderr "Too many files. Use -i for directories.")
         42)
        (else
         (when (not (sys/directory? out))
           (sys/mkdir out #o755))

         (let walk ((files
                     (filter
                      (λ (f) (let ((p (full-path in f)))
                               (and (sys/file? p) (scheme? p))))
                      (sys/dir->list in))))
           (if (null? files)
               0
               (let* ((inp (full-path in (car files)))
                      (outp (full-path out (scheme-ext->js-ext (car files)))))
                 (list->file (eval-file inp) outp)
                 (format stdout "~a -> ~a~%" inp outp)
                 (walk (cdr files)))))))))))
