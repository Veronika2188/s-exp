(define (make-atom value)
  (list 'atom value))

(define (make-list children)
  (list 'list children))

(define (sexp-to-string sexp)
  (cond ((eq? (car sexp) 'atom) (cadr sexp))
        ((eq? (car sexp) 'list)
         (string-append "(" (apply string-append (map sexp-to-string (cadr sexp))) ")"))
        (else "")))

(define (tokenize input)
  (let ((input (string-replace input "(" " ( "))
        (input (string-replace input ")" " ) ")))
    (filter (lambda (s) (not (string=? s ""))) (string-split input " "))))

(define (parse-tokens tokens)
  (if (null? tokens)
      (error "Unexpected end of input")
      (let ((token (car tokens))
            (rest (cdr tokens)))
        (cond ((string=? token "(")
               (let-values (((children remaining) (parse-list rest)))
                 (values (make-list children) remaining)))
              ((string=? token ")")
               (error "Unmatched )"))
              (else (values (make-atom token) rest))))))

(define (parse-list tokens)
  (cond ((null? tokens) (error "Unmatched ("))
        ((string=? (car tokens) ")") (values '() (cdr tokens)))
        (else
         (let-values (((sexp remaining) (parse-tokens tokens)))
           (let-values (((sexps final-remaining) (parse-list remaining)))
             (values (cons sexp sexps) final-remaining))))))

(define (parse input)
  (let-values (((sexp remaining) (parse-tokens (tokenize input))))
    (if (null? remaining)
        sexp
        (error "Invalid S-expression"))))

(define (main)
  (let ((input "(+ 1 (* 2 3))"))
    (display (sexp-to-string (parse input)))
    (newline)))

(define (string-replace str old new)
  (string-join (string-split str old) new))

(define (string-split str delim)
  (let ((pos (string-index str delim)))
    (if pos
        (cons (substring str 0 pos)
              (string-split (substring str (+ pos (string-length delim))) delim))
        (list str))))

(main)
