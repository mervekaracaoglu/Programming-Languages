(define get-operator (lambda (op) 
	(cond 
	  ( (eq? op '+) +)
	  ( (eq? op '*) *)
	  ( (eq? op '-) -)
	  ( (eq? op '/) /)
	  ( else (display "cs305: ERROR")))))

(define define-statement? (lambda (e)
  (and 
    (list? e)              
    (= (length e) 3)       
    (eq? (car e) 'define)
    (symbol? (cadr e))   
  )))


(define get-value (lambda (var env)
  (cond
    ((null? env)
     (begin
       (display "cs305: ERROR")
       (newline)
       (newline)
       #f)) 

    ((eq? var (caar env)) (cdar env))
    (else (get-value var (cdr env))))))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))


(define s6 (lambda (e env)
  (cond 
    ((number? e) e)
    ((symbol? e) 
     (let ((val (get-value e env)))
       (if (eq? val #f)
           #f
           val)))

    ((not (list? e)) 
     (begin
       (display "cs305: ERROR")
       (newline)

       #f))

    ((not (> (length e) 0)) 
     (begin
       (display "cs305: ERROR")
       (newline)
       #f))

    ((eq? (car e) 'lambda) 
     (let ((params (cadr e))
           (body (caddr e)))
       (lambda (args)
         (let ((args-list (if (list? args) args (list args))))
           (if (not (= (length params) (length args-list)))
               (begin
                 (display "cs305: ERROR")
                 (display args-list)
                 (newline)
                 #f)
               (let ((extended-env
                      (fold-left (lambda (env-binding arg-pair)
                                   (extend-env (car arg-pair) (cdr arg-pair) env-binding))
                                 env
                                 (map cons params args-list))))
                 (s6 body extended-env)))))))

    ((eq? (car e) 'let) 
     (let ((bindings (cadr e))
           (body (caddr e)))
       (if (not (and (list? bindings)
                     (every (lambda (b)
                              (and (pair? b)
                                   (symbol? (car b))
                                   (= (length b) 2)))
                            bindings)))
           (begin
             (display "cs305: ERROR")
             (newline)
             (newline)
             #f)
           (let ((extended-env
                  (fold-left (lambda (current-env binding)
                               (extend-env (car binding) (s6 (cadr binding) env) current-env))
                             env
                             bindings)))
             (if (not body)
                 (begin
                   (display "cs305: ERROR")
                   (newline)
                   #f)
                 (s6 body extended-env))))))

    (else 
     (let ((operator (s6 (car e) env)))
       (if (not (procedure? operator))
           (begin
             #f)
           (let ((operands (map (lambda (x) (s6 x env)) (cdr e))))
             (if (member #f operands)
                 #f
                 (apply operator operands)))))))))



(define initial-env
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)))



(define cs305 (lambda ()
  (let loop ((env initial-env))
    (display "cs305> ")
    (let ((expr (read)))
      (cond
        ((define-statement? expr)
         (let ((var (cadr expr))
               (val (s6 (caddr expr) env)))
           (let ((new-env (extend-env var val env)))
             (display "cs305: ")
             (display var)
             (newline)
             (newline)
             (loop new-env))))
        ((number? expr)
         (begin
           (display "cs305: ")
           (display expr)
           (newline)
           (newline)
           (loop env)))

        ((string? expr)
         (begin
           (display "cs305: ERROR")
           (newline)
           (loop env)))

        ((symbol? expr)
         (let ((val (get-value expr env)))
           (if (eq? val #f)
               (loop env)
               (begin
                 (display "cs305: ")
                 (if (procedure? val)
                     (display "[PROCEDURE]")
                     (display val))
                 (newline)
                 (newline)
                 (loop env)))))

        ((list? expr)
         (let ((val (s6 expr env)))
           (if (eq? val #f)
               (loop env)
               (begin
                 (display "cs305: ")
                 (if (procedure? val)
                     (display "[PROCEDURE]")
                     (display val))
                 (newline)
                 (newline)
                 (loop env)))))

        (else
         (begin
           (display "cs305: ERROR")
           (newline)
           (loop env))))))))
