(define-datatype variable variable?
  (a-variable (serial number?)
              (valor list?))) 


(define create-var
  (lambda (serial valor)
    (a-variable serial (list valor))))


(define get-serial
  (lambda (var)
    (cases variable var
      (a-variable (serial valor) serial))))

(define get-valor
  (lambda (var)
    (cases variable var
      (a-variable (serial valor) valor))))

;(define isFree?
;  (lambda (var)
;    (cases variable var
;      (a-variable (serial valor)
;                 (if (equal? (car valor) '_) #t #f)))))

(define isDet?
  (lambda (var)
    (not (isFree? var))))

