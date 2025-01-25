(define extract-identifiers
  (lambda (expr)
    (cond
      ((null? expr) '())                              
      ((symbol? expr)                                
       (if (or (equal? expr 'and)                     
               (equal? expr 'or)
               (equal? expr 'not)
               (equal? expr 'xor)
               (equal? expr '=))
           '()                                       
           (list expr)))                             
      ((list? expr)                                  
       (apply append (map extract-identifiers expr)))
      (else '()))))                                  

(define find-undeclared-identifiers
  (lambda (lcd)
    (let* ((declarations (car lcd))                     
           (assignments (cadr lcd))                     
           (evaluation (caddr lcd))             
           (declared-ids (apply append (map cdr declarations)))
           (used-ids (append 
                      (apply append (map extract-identifiers assignments))
                      (apply append (map (lambda (eval) (map car (caddr eval))) evaluation)))))
      (filter (lambda (id) (not (exists-in-list? id declared-ids))) used-ids))))

(define find-multiple-declarations
  (lambda (lcd)
    (let* ((declarations (apply append (map cdr (car lcd))))) 
      (letrec ((find-duplicates (lambda (lst seen result)
                                  (if (null? lst) (reverse result)
                                      (let ((head (car lst)) (tail (cdr lst)))
                                        (if (exists-in-list? head seen)
                                            (find-duplicates tail seen (cons head result))
                                            (find-duplicates tail (cons head seen) result)))))))
        (find-duplicates declarations '() '())))))

(define exists-in-list?
  (lambda (element lst)
    (cond
      ((null? lst) #f)
      ((equal? element (car lst)) #t)
      (else (exists-in-list? element (cdr lst))))))

(define remove-duplicates
  (lambda (lst)
    (letrec ((helper (lambda (lst seen)
                       (if (null? lst) '()                         
                           (let ((head (car lst)) (tail (cdr lst)))
                             (if (member head seen)                 
                                 (helper tail seen)
                                 (cons head (helper tail (cons head seen)))))))))
      (helper lst '()))))    

(define check-unused-inputs
  (lambda (lcd)
    (let* ((declarations (car lcd))                    
           (assignments (cadr lcd))                   
           (inputs (apply append (map cdr (filter (lambda (decl) (equal? (car decl) '"input")) declarations))))
           (rhs-identifiers
             (apply append (map (lambda (assign) (if (and (list? assign) (equal? (cadr assign) '=)) (extract-identifiers (cddr assign)) '())) assignments))))
      (filter (lambda (input) (not (exists-in-list? input rhs-identifiers))) inputs))))

(define check-unassigned-nodes-outputs
  (lambda (lcd)
    (let* ((declarations (car lcd))                     
           (assignments (cadr lcd))                      
           (nodes-outputs (apply append (map cdr (filter (lambda (decl) (or (equal? (car decl) '"node") (equal? (car decl) '"output"))) declarations)))) 
           (lhs-identifiers (map car assignments)))    
      (filter (lambda (id) (not (member id lhs-identifiers))) nodes-outputs))))

(define check-multiple-assignments 
  (lambda (lcd)
    (let* ((declarations (car lcd))                     
           (assignments (cadr lcd))                    
           (valid-identifiers (apply append (map cdr declarations)))
           (lhs-identifiers (map car assignments)))    
        (letrec ((count-occurrences
                (lambda (id lst)
                  (if (null? lst) 0
                      (+ (if (equal? id (car lst)) 1 0)
                         (count-occurrences id (cdr lst))))))
               (find-multiple
                (lambda (ids)
                  (filter (lambda (id) (> (count-occurrences id lhs-identifiers) 1)) ids))))
        (find-multiple valid-identifiers)))))

(define check-identifier-usage
  (lambda (lcd)
    (list (check-unused-inputs lcd)               
          (check-unassigned-nodes-outputs lcd)     
          (check-multiple-assignments lcd))))     

(define count-occurrences
  (lambda (item lst)
    (if (null? lst) 0
        (+ (if (equal? item (car lst)) 1 0)
           (count-occurrences item (cdr lst))))))

(define find-unassigned-inputs
  (lambda (inputs evaluations)
    (map car
         (filter (lambda (eval)
                   (let ((assigned-inputs (cadr eval)))
                     (not (null? (filter (lambda (input) (not (member input assigned-inputs))) inputs))))) evaluations))))

(define find-multiple-assigned-inputs
  (lambda (inputs evaluations)
    (map car
         (filter (lambda (eval)
                   (let ((assigned-inputs (cadr eval)))
                     (not (null? (filter (lambda (input)
                                           (> (count-occurrences input assigned-inputs) 1)) inputs))))) evaluations))))

(define check-inputs-in-evaluation
  (lambda (lcd)
    (let* ((declarations (car lcd))                     
           (evaluations (caddr lcd))                  
           (inputs (apply append
                         (map cdr (filter (lambda (decl) (equal? (car decl) '"input")) declarations))))
           (evaluation-assignments
             (map (lambda (eval) (list (cadr eval) (map car (caddr eval)))) evaluations)))
      (list (find-unassigned-inputs inputs evaluation-assignments)
            (find-multiple-assigned-inputs inputs evaluation-assignments)))))

(define extract-inputs
  (lambda (declarations)
    (apply append
           (map cdr (filter (lambda (decl)
                              (equal? (car decl) '"input"))
                            declarations)))))

(define find-incorrect-input-assignments
  (lambda (inputs assignments)
    (let loop ((lst assignments) (result '()))
      (if (null? lst)
          result
          (let ((head (car lst))
                (tail (cdr lst)))
            (if (exists-in-list? (car head) inputs) 
                (loop tail (cons (car head) result)) 
                (loop tail result)))))))

(define check-incorrect-input-assignments
  (lambda (lcd)
    (let* ((declarations (car lcd)) 
           (assignments (cadr lcd)) 
           (inputs (extract-inputs declarations)))  
      (find-incorrect-input-assignments inputs assignments)))) 

(define extract-nodes-outputs
  (lambda (declarations)
    (apply append
           (map cdr (filter (lambda (decl)
                              (or (equal? (car decl) '"node")
                                  (equal? (car decl) '"output")))
                            declarations)))))

(define find-incorrect-node-output-assignments
  (lambda (nodes-outputs evaluations)
    (let loop ((lst evaluations) (result '()))
      (if (null? lst)
          result
          (let ((head (car lst))
                (tail (cdr lst)))
            (let ((assigned-ids (map car (caddr head))))
              (loop tail (append result (filter (lambda (id) (exists-in-list? id nodes-outputs)) assigned-ids)))))))))

(define check-incorrect-node-output-assignments
  (lambda (lcd)
    (let* ((declarations (car lcd))  
           (evaluations (caddr lcd)) 
           (nodes-outputs (extract-nodes-outputs declarations))) 
      (find-incorrect-node-output-assignments nodes-outputs evaluations)))) 

(define check-incorrect-assignments
  (lambda (lcd)
    (let* ((declarations (car lcd))        
           (assignments (cadr lcd))         
           (evaluations (caddr lcd))       
           (inputs (extract-inputs declarations))
           (nodes-outputs (extract-nodes-outputs declarations)))
      (list (find-incorrect-input-assignments inputs assignments)       
            (find-incorrect-node-output-assignments nodes-outputs evaluations)))))
