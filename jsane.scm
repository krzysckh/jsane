(import
 (owl toplevel)
 (owl args)
 (owl sexp)
 (prefix (owl sys) sys/))

;; TODO: i really want this to be a shitty cps

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

(define (jsize-symbol s) ; B
  (fold
   (λ (a b)
     (if-lets ((v (assoc b jsize-rename-alist))) 
       (str a (cdr v))
       (str a (string b))))
   ""
   (string->runes s)))

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

(define (commize vs)
  (cond
   ((null? vs) "")
   ((null? (cdr vs)) (car vs))
   (else
    (fold
     (λ (a b) (str a "," b))
     (car vs)
     (cdr vs)))))

(define (maybe-rewrite-fn v)
  (if (has? infix v)
      v
      (if-lets ((newsym (assoc v fn-rewrite-alist)))
        (cdr newsym)
        (jsize-symbol (str v)))))

(define (infix? v)
  (or (has? infix v) (has? infix (maybe-rewrite-fn v))))

(define (jsize-thing v)
  (cond
   ((number? v) v)
   ((string? v) (str* v))
   ((symbol? v) (maybe-rewrite-fn v))
   ((list? v) (str "[" (commize (map jsize-thing v)) "]"))

   (else
    (error "unknown thing " v))))

;; macro = ff of
;;  name
;;  args
;;  body
(define (defmacro? e)
  (and (list? e)
       (or
        (eqv? (car e) 'defmacro)
        (eqv? (car e) 'define-macro))))

(define (macro-letize exp args body)
  (cond
   ((null? args) body)
   ((pair? args)
    `(let ((,(car args) ',(car exp)))
       ,(macro-letize (cdr exp) (cdr args) body)))
   (else
    `(let ((,args ',exp))
       ,(macro-letize #n #n body)))))

(define (apply-macro macro exp walk)
  (let* ((name (get macro 'name #f))
         (args (get macro 'args #f))
         (body (get macro 'body #f))
         (code (macro-letize (cdr exp) args body)))
    (walk (eval code *toplevel*))))

(define (walk-apply-macros macros exp)
  (let* ((names (map (λ (m) (get m 'name #f)) macros))
         (ms (zip cons names macros)))
    (let walk ((exp exp))
      (cond
       ((null? exp)
        #n)
       ((not (list? exp))
        exp)
       ((has? names (car exp))
        (apply-macro (cdr (assoc (car exp) ms)) exp walk))
       (else
        (map walk exp))))))

(define (macro-of exp)
  (let* ((exps (cadr exp))
         (name (car exps))
         (args (cdr exps))
         (body (caddr exp)))
    (pipe empty
      (put 'name name)
      (put 'args args)
      (put 'body body))))

;; forms -> forms'
(define (apply-macros forms)
  (let* ((defmacros     (filter defmacro? forms))
         (not-defmacros (filter (B not defmacro?) forms))
         (macros (map macro-of defmacros)))
    (map (H walk-apply-macros macros) not-defmacros)))

(define core
  '((define-macro (let lst . body)
      (if (null? lst)
          `(begin
             ,@body)
          `((λ (,(caar lst)) (let ,(cdr lst) ,@body)) ,(cadar lst))))
    ))

(define-syntax js_
  (syntax-rules (λ begin define quote if when list-ref ref _raw set! spread spreading return new while object =>)
    ((_ (object (key => value) ...) . rest)
     (str
      "({" (str (_ key) ":" (_ value) ",") ... "})"
      (_ . rest)))
    ((_ (while test . do) . rest)
     (str
      "while (" (_ test) ")" (_ (begin . do))
      (_ . rest)))
    ((_ (new T arg ...) . rest)
     (str
      "(new " (_ T) "(" (commize (list (_ arg) ...)) "))"
      (_ . rest)))
    ((_ (return it) . rest)
     (str
      "return " (_ it)
      (_ . rest)))
    ((_ (spread lst) . rest)
     (str
      "[..." (_ lst) "]"
      (_ . rest)))
    ((_ (spreading lst) . rest)
     (str
      "..." (_ lst) ""
      (_ . rest)))
    ((_ (set! place val) . rest)
     (str
      (_ place) " = " (_ val) ";"
      (_ . rest)))
    ((_ (_raw string) . rest)
     (str string (_ . rest)))
    ((_ (ref obj key) . rest)
     (str
      (_ obj) "." (_ key)
      (_ . rest)))
    ((_ (list-ref lst n) . rest)
     (str
      (_ lst) "[" (_ n) "]"
      (_ . rest)))
    ((_ (begin) . rest)
     (_ . rest))
    ((_ (begin exp1) . rest)
     (str
      "(() => { return " (_ exp1) "})();"
      (_ . rest)))
    ((_ (begin exp1 . exp) . rest)
     (str
      "(() => {" (_ exp1) "; return " (_ (begin . exp)) "})();"
      (_ . rest)))
    ((_ (when test . then) . rest)
     (_ (if test (begin . then)) . rest))
    ((_ (if test then else) . rest)
     (str
      "(() => { if (" (_ test) ") { return " (_ then) " } else { return " (_ else) " } })()"
      (_ . rest)))
    ((_ (if test then) . rest)
     (str
      "(() => { if (" (_ test) ") { return " (_ then) " } })()"
      (_ . rest)))
    ((_ (define (name . args) . code) . rest)
     (_ (define name (λ args . code)) . rest))
    ((_ (define name . code) . rest)
     (str
      "\nconst " (_ name) " = " (_ . code) ";"
      (_ . rest)))
    ((_ (λ (arg ...) . code) . rest)
     (str
      "((" (commize (list (_ arg) ...)) ") => { " (_ (return (begin . code))) " })"
      (_ . rest)))
    ((_ (quote thing) . rest)
     (str (jsize-thing 'thing)
          (_ . rest)))
    ((_ (f arg1 arg2) . rest)  ; only check for infix operators in funcalls with 2 args, sorry in advance :/
     (str
      (if (infix? 'f)
          (str "((" (_ arg1) ")" (_ 'f) "(" (_ arg2) "))")
          (str (_ f) "(" (commize (list (_ arg1) (_ arg2))) ")")) ; this repeats >---+
      (_ . rest)))                                                ;                  |
    ((_ (f arg ...) . rest)                                       ;                  |
     (str (_ f) "(" (commize (list (_ arg) ...)) ")"              ; this <-----------+
          (_ . rest)))
    ((_ atom . rest)
     (str (jsize-thing 'atom)
          (_ . rest)))
    ((_) "")
    ))

(define-syntax js
  (syntax-rules (*toplevel*)
    ((_ exp ...)
     (eval (append '(js_) (apply-macros (append core '(exp ...)))) *toplevel*))))

(define lib
  (string->list
   (js
    (define jsane--lib--loaded "indeed")

    (define print console.log)
    (define (car lst)   (list-ref lst 0))
    (define (cdr lst)   ((ref lst slice) 1))
    (define (length ob) (ref ob 'length))
    (define len length)
    (define (null? ob)  (eq? (length ob) 0))
    (define (map f lst) ((ref lst map) f))
    (define (take l n)  ((ref l 'slice) 0 n))
    (define (drop l n)  ((ref l 'slice) n))

    (define list Array)
    (define (element! type) (document.createElement type))

    (define fold (λ (f a b)
                   (if (null? b)
                       a
                       (fold f (f a (car b)) (cdr b))))))))

;; TODO: this assumes lib.js in the same dir and also named lib.js
(define maybe-load-lib
  (string->list
   (str
    "let when_ready; let when_jsane_ready;"
    (js
     (begin
       (_raw "let jsane__ready_p = false;\n")

       (define (jsane--load--lib!)
         (let ((script (document.createElement "script"))
               (cb (λ ()
                     (set! jsane__ready_p true))))
           (set! script.type "text/javascript")
           (set! script.src "lib.js")
           (document.head.appendChild script)

           (set! script.onreadystatechange cb)
           (set! script.onload cb)))

       (define (_when-jsane-ready f)
         (if jsane__ready_p
             (f)
             (setTimeout (λ () (when-jsane-ready f)) 100)))

       (set! when_ready _when-jsane-ready)
       (set! when_jsane_ready _when-jsane-ready)

       (when (eqv? (typeof jsane--lib--loaded) "undefined")
         (jsane--load--lib!))
       )))))

(define scheme? (string->regex "m/\\.scm$/"))
(define scheme-ext->js-ext (string->regex "s/\\.scm$/.js/"))

(define command-line-rules
  (cl-rules
   `((help     "-h" "--help")
     (no-lib   "-n" "--no-library" comment "don't load the builtin library")
     (input    "-i" "--input"  has-arg comment "input file directory (not recursive)")
     (output   "-o" "--output" has-arg comment "target directory for js files" default "public"))))

(define (full-path d s)
  (format #f "~a/~a" d s))

(define (eval-file path)
  (let* ((lst (if (port? path) (force-ll (port->byte-stream path)) (file->list path)))
         (code `(let ((*toplevel* ,*toplevel*)) ,(append '(js) (list->sexps lst print "syntax-error"))))
         (res (eval code *toplevel*)))
    (string->list res)))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print-rules command-line-rules)
       (halt 0))

     (let ((in (get opt 'input #f))
           (out (get opt 'output #f))
           (no-lib (get opt 'no-lib #f)))
       (cond
        ((= (length extra) 1)         (write-bytes stdout (eval-file (car extra))) 0)
        ((and (null? extra) (not in)) (write-bytes stdout (eval-file stdin)) 0)
        ((> (length extra) 1)
         (print-to stderr "Too many files. Use -i for directories.")
         42)
        (else
         (when (not (sys/directory? out))
           (sys/mkdir out #o755))

         (unless no-lib
           (list->file lib (str out "/lib.js")))

         (let walk ((files
                     (filter
                      (λ (f) (let ((p (full-path in f)))
                               (and (sys/file? p) (scheme? p))))
                      (sys/dir->list in))))
           (if (null? files)
               0
               (let* ((inp (full-path in (car files)))
                      (outp (full-path out (scheme-ext->js-ext (car files)))))
                 (list->file (append (if no-lib #n maybe-load-lib) (eval-file inp)) outp)
                 (format stdout "~a -> ~a~%" inp outp)
                 (walk (cdr files)))))))))))
