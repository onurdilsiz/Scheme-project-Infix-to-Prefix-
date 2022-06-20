; onur dilsiz
; 2019400036
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value ) ) )
;(:= 'x 5)
; 10 points
;(define -- (lambda (args ) 

(define -- (lambda args (list 'let  args       )  ))


; 10 points
(define @ (lambda (bindings expr) (append bindings expr)))

 


; 20 points

( define split_at_delim (lambda (delim args ) (foldr (lambda (1st restofargs )
           (cond ((eqv? 1st delim)
               (cons '() restofargs))
           (else (cons (cons 1st (car restofargs)) (cdr restofargs)))))
         (list '()) args ))
)



(define (split_ass args) (if (not (empty? args))(:= (if (list? (caar(split_at_delim ':= args))) (cadaar(split_at_delim ':= args) )
                                                        (caar(split_at_delim ':= args))) (if(not (number? (caadr(split_at_delim ':= args)))) (car(cdaadr(split_at_delim ':= args))) (caadr(split_at_delim ':= args)))) args))



(define parse_expr (lambda (expr) (if(member '+ expr) (cons '+ (parse_expr(split_at_delim '+ expr)))
                                     (if (member '* expr) (cons '* (parse_expr(split_at_delim '* expr)))
                                         (if (member '@ expr) (list  'let (map split_ass (remove '()(split_at_delim '-- (remove '()(caar(split_at_delim '@ expr))))))
                                                                 
                                                                    (parse_expr (caadr (remove '() (split_at_delim '@ expr) ))))
                                         
                                               
                                         (if(and (list? expr) (number? (car expr)) (= (length expr) 1 )) (car expr)
                                            (if(and (list? expr) (member  (car expr) '( a b c d e f g h i j k l m n o p q r s t u y v z w x )) (= (length expr) 1 )) (car expr)
                                            (if(and (list? expr) (= (length expr) 1 )) (parse_expr(car expr))
                                               
                                               (map parse_expr  expr)
                                               )
                                             ))
                                         )
                                         
                                         )
                                     )

                 ))


; 20 points
(define eval_expr (lambda (expr) (eval (parse_expr expr ) (make-base-namespace))) )
