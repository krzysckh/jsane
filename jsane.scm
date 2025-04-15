(import
 (owl toplevel)
 (owl args)
 (prefix (owl sys) sys/))

;; TODO: i really want this to be a shitty cps
;; TODO: i also want to find the last exp per function, as (return)-ing in scheme is sacrilege

(define jsize-1 (string->regex "s/[\\-*!@#$%^&*]/_/g"))
(define jsize-2 (string->regex "s/\\?$/_p/g"))

(define (jsize-symbol s) ; B
  (jsize-1 (jsize-2 s)))

(define infix '(+ - / // * ** % = == === != !== && ||))

(define (commize vs)
  (cond
   ((null? vs) "")
   ((null? (cdr vs)) (car vs))
   (else
    (fold
     (λ (a b) (str a "," b))
     (car vs)
     (cdr vs)))))

(define (jsize-thing v)
  (cond
   ((number? v) v)
   ((string? v) (str* v))
   ((symbol? v) (if (eqv? v '=)
                    "=="
                    (jsize-symbol (str v))))
   ((list? v) (str "[" (commize (map jsize-thing v)) "]"))

   (else
    (error "unknown thing " v))))

(define-syntax js
  (syntax-rules (λ _let begin define quote if when list-ref ref _raw)
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
    ((_ (begin exp1 . exp) . rest)
     (str
      (_ exp1) ";"
      (_ (begin . exp) . rest)))
    ((_ (when test . then) . rest)
     (_ (if test (begin . then)) . rest))
    ((_ (if test then else) . rest)
     (str
      "if (" (_ test) ") { " (_ then) " } else { " (_ else) " }"
      (_ . rest)))
    ((_ (if test then) . rest)
     (str
      "if (" (_ test) ") { " (_ then) " } "
      (_ . rest)))
    ((_ (define (name . args) . code) . rest)
     (_ (define name (λ args . code)) . rest))
    ((_ (define name . code) . rest)
     (str
      "\nconst " (_ name) " = " (_ . code) ";"
      (_ . rest)))
    ((_ (λ (arg ...) . code) . rest)
     (str
      "((" (commize (list (_ arg) ...)) ") => { " (_ (begin . code)) " })"
      (_ . rest)))
    ((_ (_let 42 () . code))
     (_ . code))
    ((_ (_let 42 ((name value) . vars) . code))
     (_ ((λ (name)
           (_let 42 vars . code)) value)))
    ((_ (let ((uh um) ...) . code) . rest)
     (str
      (_ (_let 42 ((uh um) ...) . code))
      (_ . rest)))
    ((_ (quote thing) . rest)
     (str (jsize-thing 'thing)
          (_ . rest)))
    ((_ (f arg1 arg2) . rest)  ; only check for infix operators in funcalls with 2 args, sorry in advance :/
     (str
      (if (has? infix 'f)
          (str "(" (_ arg1) ")" (_ 'f) "(" (_ arg2) ")")
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

;; (print
;;  (js
;;   (define print console.log)
;;   (define (car lst)   (return (list-ref lst 0)))
;;   (define (cdr lst)   (return ((ref lst splice) 1)))
;;   (define (null? ob)  (return (= (ref ob length) 0)))
;;   (define (map f lst) (return ((ref lst map) f)))

;;   (define (iota n) (return (_raw "[...Array(n).keys()]")))

;;   (define fold (λ (f a b)
;;                  (if (null? b)
;;                      (return a)
;;                      (return (fold f (f a (car b)) (cdr b))))))

;;   (define (main)
;;     (print (fold (λ (a b) (return (+ a b))) 0 (Array 1 2 3)))
;;     (print (iota 10))
;;     (print (map (λ (x) (return (+ x 1))) (iota 10)))
;;     )

;;   (main)
;;   ))

(define scheme? (string->regex "m/\\.scm$/"))
(define scheme-ext->js-ext (string->regex "s/\\.scm$/.js/"))

(define command-line-rules
  (cl-rules
   `((help     "-h" "--help")
     (input    "-i" "--input"  has-arg comment "input file directory (not recursive)")
     (output   "-o" "--output" has-arg comment "target directory for js files" default "public"))))

(define (full-path d s)
  (format #f "~a/~a" d s))

(define (eval-file path)
  (let* ((f (if (port? path) path (open-input-file path)))
         (res (eval (read f) *toplevel*)))
    (when (not (port? path))
      (close-port f))
    (string->list res)))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print-rules command-line-rules)
       (halt 0))

     (let ((in (get opt 'input #f))
           (out (get opt 'output #f)))
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
