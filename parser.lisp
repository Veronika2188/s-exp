(defstruct sexp
  (type nil)
  (value nil))

(defun make-atom (value)
  (make-sexp :type 'atom :value value))

(defun make-list (children)
  (make-sexp :type 'list :value children))

(defun sexp-to-string (sexp)
  (cond ((eq (sexp-type sexp) 'atom) (sexp-value sexp))
        ((eq (sexp-type sexp) 'list)
         (concatenate 'string "(" (apply #'concatenate 'string (mapcar #'sexp-to-string (sexp-value sexp))) ")"))
        (t "")))

(defun tokenize (input)
  (let ((input (regex-replace-all "(" input " ( "))
        (input (regex-replace-all ")" input " ) ")))
    (remove "" (split-sequence " " input) :test #'string=)))

(defun parse-tokens (tokens)
  (if (null tokens)
      (error "Unexpected end of input")
      (let ((token (first tokens))
            (rest (rest tokens)))
        (cond ((string= token "(")
               (multiple-value-bind (children remaining)
                   (parse-list rest)
                 (values (make-list children) remaining)))
              ((string= token ")")
               (error "Unmatched )"))
              (t (values (make-atom token) rest))))))

(defun parse-list (tokens)
  (cond ((null tokens) (error "Unmatched ("))
        ((string= (first tokens) ")") (values nil (rest tokens)))
        (t (multiple-value-bind (sexp remaining)
               (parse-tokens tokens)
             (multiple-value-bind (sexps final-remaining)
                 (parse-list remaining)
               (values (cons sexp sexps) final-remaining))))))

(defun parse (input)
  (multiple-value-bind (sexp remaining)
      (parse-tokens (tokenize input))
    (if (null remaining)
        sexp
        (error "Invalid S-expression"))))

(defun main ()
  (let ((input "(+ 1 (* 2 3))"))
    (format t "~a~%" (sexp-to-string (parse input)))))

(main)

(defun regex-replace-all (regex input replacement)
  (cl-ppcre:regex-replace-all regex input replacement))

(defun split-sequence (delimiter input)
  (cl-ppcre:split delimiter input))
