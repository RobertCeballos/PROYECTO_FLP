(load "variables.scm")
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::=
;;  <primitive>     ::= 


;POSIBLES FUNCIONES A ELIMINAR isFree?,  apply-env-env, revisar orelse 
;******************************************************************************************

;*******************************ESPECIFICACION LEXICA**********************************
(define scanner-spec-simple-interpreter
'((white-sp(whitespace) skip)
  (comment("%" (arbno (not #\newline))) skip)
  ;(variable ((or "_" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "Ñ" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z") 
   ;            (arbno (or letter digit "?")) ) symbol)
   (variable ((or (or "_" "$") (concat (or "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
                           (arbno (or letter digit "_"))))) symbol) 
  
   (atomo  ((or "a" "b" "c" "ch" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "ñ" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" )
          (arbno  (or letter digit ) ))symbol)
          ;( (arbno "'" letter "'") )) ) symbol)
   
     
   
          
  
  (entero (digit (arbno digit)) string)
  (entero ("~" (arbno digit)) string)
  (flotante (digit (arbno digit) "." digit (arbno digit)) string)
  (flotante ("~" digit (arbno digit) "." digit (arbno digit))string)
  ))

;*************************************GRAMATICA*******************************************
;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    
    (program (expression) a-program)
    
    (cuerpo (expression (arbno expression)) cuerpoc)
    
    (expression (variable) var-exp)
    
    (expression (entero) entero-exp)
    
    (expression (flotante) flotante-exp)
    
    
    (expression (primitive "{" (arbno expression)"}") primapp-exp)
    
    (expression ("local"  (arbno variable) "in" cuerpo "end") local-exp)
    
;    (expression ( "{" primitive (arbno expression)"}")
;                primcell-exp)
;    
   ; (expression ( primitive expression) check-cell)
    
    (expression ("set" expression "=" expression)set-exp)
    
    (expression ("for" (arbno variable) "in" expression ".." expression
                       "do" cuerpo "end") for-exp)
     
    (expression ("[" (arbno expression) "]")list-exp)
    
    (expression (atomo "(" (arbno atomo ":" expression) ")") record-exp)
    
    (expression ("." variable "." atomo) acc-camp-reg)
    
    ;PROCEDIMIENTOS
    
    
    (expression ("proc" "{" variable  (arbno variable) "}" cuerpo "end") proc-exp)    
    
    (expression ("{" variable (arbno expression)"}") app-exp)
    
    ;FUNCIONES
    
    (expression ("fun" "{" variable  (arbno variable) "}" cuerpo "end") fun-exp)    
    
    ;PATRONES
    
    (patron ( atomo "("(arbno atomo ":" patron ")" ) )pat-exp)
    (patron ( variable) var-pat)
    (patron (entero) enterot-pat)
    (patron (flotante) float-pat)
    
    ;EMPTY
    (empty ("#Void") empty-exp) 
     
    ;CONDICIONALES
    
    (expression ("if"  expression "then" cuerpo "else" cuerpo "end") if-exp)
    
    ;BOOLEANOS
    (expression (boolean) bool-exp)
    (boolean ("true") true-exp)
    (boolean ("false") false-exp)
    
    ;PRIMITIVAS
    ;Aritmeticas
    (primitive ("+") sum-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    
    ;Comparacion
    (primitive ("<") menor-prim)
    (primitive ("=<") meneq-prim)
    (primitive (">") mayor-prim)
    (primitive (">=") mayig-prim)
    (primitive ("==") igual-prim)
    
    ;Celdas
    (primitive ("newcell") newcell-prim)
    (primitive ("iscell?") iscell-prim)
    (primitive ("@") valcell-prim)
    (primitive ("setcell") setcell-prim)
    
    ;puertos
    (primitive ("newport") newport-prim)
    (primitive ("isport?") isport-prim)
    (primitive ("send") sendP-prim)
    (primitive (",") valport-prim)
    
    ;Logicas
    (primitive ("orelse") orelse-prim)
    (primitive ("andthen") andthen-prim)
    
    ;Unificacion
    (primitive ("=") unif-prim)
    
    ;Prim-variables
    (primitive ("isfree?") isfree-prim)
    (primitive ("isdet?") isdet-prim)
    ))


;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define i
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;****************************************INTERPRETE****************************************
;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (let ((resultado
           (cases program pgm
             (a-program (body)
                        (eval-expression body (init-env))))))
            (print resultado)
      )))


(define print
  (lambda (resultado)
    (if (number? resultado) (unparse resultado)
        (if (variable? resultado) 
          (cases variable resultado
            (a-variable (serial valor)
                        (print(car(apply-store serial)))))
          resultado))))

;Env inicial      
(define init-env
  (lambda ()
    (extend-env '(A B C)'(0 1 2)(empty-env)))) 

;Store inicial
(define init-store
  (let ((store (make-vector 1))
        (A (a-variable 0 (list 5) ))
        (B (a-variable 1 (list -10)))
        (C (a-variable 2 (list 15))))
    (vector-set! store 0 (list A B C))
    store
    ))

;*******************************************************************************************
;****************************************AMBIENTES******************************************

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))



;**********************************CELDAS********************************************

;definición del tipo de dato celda
 (define-datatype celda celda?
       (a-cell (val vector?)))
              
 
 
 ;**********************************PUERTOS********************************************

;definición del tipo de dato puerto
 (define-datatype puerto puerto?
       (a-port (val vector?)))
 
 
 
;**********************************REGISTROS*****************************************
 ;Definición tipo de dato REGISTRO
(define-datatype registro registro?
  (reg-vacio (etiq symbol?))
  (reg-datos (etiq symbol?)
             (campos (list-of symbol?))
             (vals vector?)))

;**************************************************************************************
;;************************************Expresiones************************************
;eval-expression

(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      
      (entero-exp (ent) (parse-to-number ent))
      
      (flotante-exp (flt)(parse-to-number flt)) 
      
     (var-exp (id) 
               (if (equal? id '_) (update-store2 (create-var  (length(vector-ref init-store 0))  id ) )
                     (let ((serial (apply-env env id)))
                                (create-var serial id))))
      
     ; (a-var-exp () create-a-var())
      

      (list-exp (exps)
                ())
      
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      
      (record-exp (etiq camp vals) 
                  (begin
                    (let ((vector-vals (make-vector 1)))
                      (vector-set! vector-vals 0 vals)
                      (reg-datos etiq camp vector-vals))))
      (proc-exp  
      (nombre vars body)
      (if( equal? nombre '$)
         (closure vars body env)
         
         (let((serial (apply-env env nombre )))
           (set-store serial (closure vars body env)))))
      
      (fun-exp
       (nombre vars body)
       (if( equal? nombre '$)
          (clase vars body env)
          (let((serial (apply-env env nombre)))
            (set-store serial (clase vars body env)))))
        
        
      (app-exp (rators rands)
               (begin 
               (let ((rator (car (apply-store (apply-env env rators))))    
                     (args (eval-rands-proc rands env)))
                 (if (procval? rator)
                    (begin
                    (apply-procedure rator args)
                    (empty-exp ))
                    (if (funval? rator)
                        (apply-procedure rator args)
                    (eopl:error 'eval-expression
                             "Attempt to apply non-function ~s" rator))))))

      (acc-camp-reg (nomReg camp) 
                    (cons-camp-reg nomReg camp env)) 
       
      (set-exp (vars exp) 
               (asignar vars exp env))
      
      (if-exp (cond-exp then-exp else-exp)
              (if (equal? (eval-expression cond-exp env) "true")
                  (eval-cuerpo then-exp env)
                  (eval-cuerpo else-exp env)))
                   
      (bool-exp (exp)
                (cases boolean exp
                  (true-exp () "true")
                  (false-exp () "false")))
       
      (local-exp (vars body) 
                 (begin
                   (let ((list-serials (asig-pos-env (length vars) (length (vector-ref init-store 0)))))
                     (save-in-store vars)
                     (eval-cuerpo body (extend-env  vars list-serials env)))))
      
      (for-exp (var var-val var-stop body)
               (let ((arg (asig-pos-env (length var) 
                    (length (vector-ref init-store 0)))))
                                 
          (let (( var-v (eval-expression var-val env)))
          (let (( var-s (eval-expression var-stop env)))
            
                (let ((varv (car (apply-store (get-serial var-v)))))
                (let ((vars (car (apply-store (get-serial var-s))))) 
                  (update-store var-v)
;                 (if (variable? var-v)(eopl:error 'apply-env "Noooooo binding for ~s" (length (vector-ref init-store 0)))
        (for-exp-aux  (car var)
                      varv 
                      vars 
                      body
                     (extend-env var arg env) 
                     (car arg))
                  ))))))
                            
      )))  
      
    

    
;;******************************************************************************************
;local {I F} in
;set I =1
;set F =9
;for Y in I .. F do
;+{Y Y}
;end
;end
(define get-serial
  (lambda (var)
    (cases variable var
      (a-variable (serial valor) serial))))

;(define get-serial-cell
;  (lambda (var)
;    (cases celda var
;      (a-cell (serial valor) serial))))

(define get-valor
  (lambda (var)
    (if(variable? var)
    (cases variable var
      (a-variable (serial valor) valor)) 
     (if (celda? var)
        (cases celda var
         (a-cell (valor) (vector-ref valor 0)))
        (if (puerto? var)
            (cases puerto var
              (a-port(valor)(vector-ref valor 0))))))))
 
(define for-result 0) 

;;;FUNCION aUXILIaR DE (FOR) REaLIZa LaS ITERaCIONES 
(define for-exp-aux
    (lambda (var ini fin body env arg)
;      (primitive-setref!
;       (apply-env-ref env var) arg)
      (set-store arg ini)
;      (begin
;        (setref!
;         (apply-env-ref env var)
;         (eval-expression fin env))
;        1)
      (cond 
        ((check-for ini fin)
         (begin
           (set! for-result  (eval-cuerpo body env))
           (let ((ref (apply-env-ref env var)))
            
           (for-exp-aux var (+ ini 1) fin body env arg))))
        (else for-result))))

;;; CHEKEa La CONDICION DE PaRaDa DEL (FOR)
(define check-for
  (lambda (ini fin)
    (if (> ini fin)#f
        #t)))

(define-datatype reference reference?
    (a-ref (position integer?)
           (vector vector?)))

;;; aSIGNa UN VaLOR a UNa REFERENCIa
(define primitive-setref!
    (lambda (ref value)
      (cases reference ref
        (a-ref (pos vec) (vector-set! vec pos value)))))

;;;CREa UNa REFERENCIa DE UN VaLOR EN EL aMBIENTE
(define apply-env-ref
    (lambda (env sym)
      (cases environment env
        (empty-env-record ()
                          (eopl:error 'apply-env "No binding for ~s" sym))
        (extended-env-record (syms vec old-env)
                             (let ((pos (list-find-position sym syms)))
                               (if (number? pos)
                                   (a-ref pos vec)
                                   (apply-env-ref old-env sym)))))))



;*******************************************************************************************************
;FUNCIONES AUXILIARES eval-expression

(define eval-cuerpo
  (lambda (body env)
    (cases cuerpo body
      (cuerpoc (primero resto)
               (if (null? resto) (eval-expression primero env)
                   (begin 
                     (eval-expression primero env)
                     (eval-cuerpo (cuerpoc (car resto) (cdr resto)) env)))))))
           
 

;Funcion que aplica update-store por cada variable
(define save-in-store
  (lambda (vars)
    (map (lambda (x) (update-store (create-var  (length(vector-ref init-store 0))  '_ ) )) vars)))


(define save-in-store-cell
  (lambda (vars num)
    (map (lambda (x) (update-store (create-cell  (length(vector-ref init-store 0))  num ) )) vars)))

                 
;Funcion que asigna posiciones en un ambiente (local{X} in X end
;ej: si init-store tiene 3 elementos  y le quiero agregar 2 al ambiente, entoncs cant=2 posF=3
(define asig-pos-env
  (lambda (cant posF)
    (if (equal? posF (+(length (vector-ref init-store 0)) cant)) ()
        (cons  posF  (asig-pos-env cant (+ posF 1)))))) 
 
;Funcion que de acuerdo al tipo de elemento realiza una asignacion (cambio en esta funcion)


(define asignar
  (lambda (var1 var2 env)
     (let ((var1 (eval-expression var1 env))
           (var2 (eval-expression var2 env)))
       
       (if (or( empty? var1) (empty? var2))
           
           (if( empty? var1)
              (eopl:error 'asignar "no se puede asignar una variable al resultado de un procedimiento ~s" var1)
              (eopl:error 'asignar "no se puede asignar una variable al resultado de un procedimiento ~s" var2))
           
           (if (or (procval? var1) (procval? var2)) ;; Si alguna de las variables o expresiones son un procedimiento se asigna a la variable en el valor si esta libre
               (asig-procval-var var1 var2 env) 
               
               (if(or (funval? var1)(funval? var2));; Si alguna de las variables o expresiones son una funcion se asigna a la variable en el valor si esta libre
                  (asig-funval-var var1 var2 env)
                  
                  (if (or (celda? var1) (celda?  var2))
         ;(eopl:error 'apply-primitive "noooooooooo ~s"  (get-valor var2))
                      (asig-var-cel var1 var2 env)
                      
                      (if (or (puerto? var1)(puerto? var2))
                          (asig-var-port var1 var2 env) 

;        ;;terminar condicionnnn
                          (cond
                            ((and (variable? var1) (variable? var2))
                             (cond
                               ((and (symbol? (car (get-valor var1))) (registro? (car (get-valor var2)))(asig-var-reg var1 var2 env))) ;variable-registro (aqui no llega)
                               ((or (and (symbol? (car (get-valor var1))) (symbol? (car (get-valor var2)))) ; variable-variable
                                    (and (number? (car (get-valor var1))) (symbol? (car (get-valor var2)))) ;campoReg-var
                                    (and (symbol? (car (get-valor var1))) (number? (car (get-valor var2)))) ;var-campoReg
                                    (and (number? (car (get-valor var1))) (number? (car (get-valor var2))))) ;campoReg-campoReg
                                (asig-var-var var1 var2 env))
                               ))
                            ((or(number?  var1) (number? var2)) (asig-var-num var1 var2));variable-numero o numero-variable
                            ((and (registro?  var1) (symbol? (car (get-valor var2))) )(asig-var-reg var2 var1 env)) ; registro-variable
                            ((and (symbol? (car (get-valor var1))) (registro?  var2))(asig-var-reg var1 var2 env)) ; variable-registro
               )))))))))
 
;asignar una variable a un puerto

(define asig-var-port
  (lambda(var1 var2 env)
    (if (isFree? (get-serial var1))
   (let ((varPort   
          (create-var (get-valor var2) var2)))
     (if (isFree? (get-serial  var1))
         ;(eopl:error 'apply-primitive "noooooooooo ~s" (get-serial var1))))))
         (set-store (get-serial var1) varPort)))
   (eopl:error 'asig-var-port "La variable ya esta ligada ~s" var1))))
    
         
        

;Asignar una variable a una celda
(define asig-var-cel
  (lambda(var1 var2 env)
        (if (isFree? (get-serial var1))
             (let ((varCell
                  (create-var  (length(vector-ref init-store 0))var2 )))(update-store varCell)
                    (if (isFree? (get-serial var1))
                  (set-store (get-serial var1) varCell)
         )))))
;          


    
 ;;Asignar a una variable un procedimiento anonimo   

(define asig-procval-var
  (lambda(var1 var2 env)
    
    (if (procval? var1)
        
        (let ((serial1 (get-serial var2)))
          (if (isFree2? serial1)
              (set-store serial1 var1)))
        (if (procval? var2)
            (let ((serial1 (get-serial var1)))
              (if (isFree2? serial1)
                  
                  (set-store serial1 var2)))
            
            (eopl:error 'asig-procval-var "La variable ya esta asignada")))))
    
       
;;Asignar a una variable una funcion anonima

(define asig-funval-var
  (lambda(var1 var2 env)
    (if (funval? var1)
        
        (let ((serial1 (get-serial var2)))
          (if (isFree2? serial1)
              (set-store serial1 var1)))
        
        (if (funval? var2)
            (let ((serial1 (get-serial var1)))
              (if (isFree2? serial1)
                  (set-store serial1 var2)))
        
        (eopl:error 'asig-procval-var "La variable ya esta asignada")))))

       ;Asignar una variable a otra variable
       
       (define asig-var-var
         (lambda(var1 var2 env)
           (let ((serial1 (get-serial var1))
                 (serial2(get-serial var2)))
             (if (or (isFree2? (get-serial var1)) (isFree2? (get-serial var2)))
                 (if (isFree2? (get-serial var1))(set-store (get-serial var1) (create-var (get-serial var2) (car (get-valor var2))))
                     (if (isFree2? (get-serial var2))(set-store (get-serial var2) (create-var (get-serial var1) (car (get-valor var1))))))
                 
                 ;Para unificar dos registros
                 (if (and (registro? (car(apply-store (get-last-ref2 (get-serial var1))))) (registro? (car(apply-store (get-last-ref2 (get-serial var2))))))
                     (if (equal? (eval-regs (car(apply-store (get-last-ref2 (get-serial var1))))  (car(apply-store (get-last-ref2 (get-serial var2)))) ) #t)
                         (unif-regs (car(apply-store (get-last-ref2 (get-serial var1)))) (car(apply-store (get-last-ref2 (get-serial var2)))))
                         (eopl:error 'asig-var "No se puede unificar")
                         )
       )))))

      
;Asignar una variable a un numero

(define asig-var-num
  (lambda (var1 var2)
    (let ((var (verificar-sym-var var1 var2 variable?))
          (num (verificar-sym-var var1 var2 number?)))
      (if (isFree2? (get-serial var))
          (set-store (get-serial var) num)
          (eopl:error 'asig-var-num "Variable ~s ya determinada" (car (get-valor var)))
      ))))


;Asignar una variable a un registro
(define asig-var-reg
  (lambda (var1 var2 env)
        (if (isFree2? (get-serial var1))
              (cases registro var2
                (reg-vacio (etiq) ())
                (reg-datos (etiq camp vals) 
                           (begin
                           (let ((regEnStore(create-reg etiq camp (vector-ref vals 0 ) env)))
                             (set-store (get-serial var1) regEnStore))))))))
                            
                          ;(apply-env-env env (car (get-valor var1)) (get-serial regEnStore) ))))))))
        


;Funcion q determina si una variable esta libre (una variable puede referenciar a otra variable)
(define isFree2?
  (lambda (pos)
    (let ((var (car(apply-store pos))))
    (if(variable? var) (isFree2? (get-serial var));(isFree2? (get-serial var))
       (if (equal? var '_) #t #f)))))



;Funcion que determina si una variable esta libre
(define isFree?
  (lambda(pos)
    (equal? (car (apply-store pos)) '_)))  

;verificar-sym-var: funcion q verifica cual de los dos elementos entrantes es el simbolo  x=7 7=x
(define verificar-sym-var
  (lambda (var1 var2 pred)
    (if (pred var1) var1
        var2)))

 
;Funcion que permite consultar el campo de un registro
(define cons-camp-reg
  (lambda (nomReg camp env)
    (let ((serialVar(apply-env env nomReg)))
      (let ((serial (get-last-ref2 serialVar)))
        (let ((reg (car (apply-store serial))))
          (if(registro? reg) 
             (let ((serialCamp(consultar-val-registro reg camp)))
               (get-last-ref serialCamp serial))
             (eopl:error 'cons-camp-reg "Error: no es un registro ~s" reg) 
             ))) )))

;Funcion que retorna la ultima referencia a una variable          
(define get-last-ref
  (lambda (serialCamp serial)
    (let ((var (car(apply-store serialCamp))))
      (if (variable? var) (get-last-ref (get-serial var) serial)
          (create-var serialCamp serial)))))

;Funcion que retorna el ultimo serial de referencia de una variable
(define get-last-ref2
  (lambda (serial)
    (let ((var (car(apply-store serial))))
      (if (variable? var) (get-last-ref2 (get-serial var))
          serial))))


;aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-rands-proc
  (lambda (rands env)
    (let ((args(map (lambda (x) (eval-expression x env)) rands))) 
           (eval-rands-proc-aux args))))  
    
    (define eval-rands-proc-aux
      (lambda (rands)
        (if (null? rands) '()
            (if (variable? (car rands)) (cons (get-serial (car rands)) (eval-rands-proc-aux (cdr rands)))
                (if (number? (car rands))
                    (let ((serial (get-serial (update-store2 (create-var (length(vector-ref init-store 0)) (car rands)) ))))
                      (cons serial (eval-rands-proc-aux (cdr rands))))
                    (eval-rands-proc-aux (cdr rands)))))))

;;******************************************************************************************
;****************************************PRIMITIVAS***************************************

;apply-primitive: <primitiva> <list-of-expression> -> numero
 
(define apply-primitive
  (lambda (prim args)
        (cases primitive prim
            (sum-prim () (let((args (to-number args)))
                           (if (equal?  (verificarTipoNumero (car args) (cdr args)) #f) (eopl:error 'apply-primitive "Error: Numeros de diferente tipo")
                               (if (null? (cdr args)) (eopl:error 'apply-primitive "Error: Minimo requiere dos operandos")
                                   (operar + args 0))
                               )))
      
          (sub-prim () (let((args (to-number args)))
                         (if (equal?  (verificarTipoNumero (car args) (cdr args)) #f) (eopl:error 'apply-primitive "Error: Numeros de diferente tipo")
                           (if (null? (cdr args)) (eopl:error 'apply-primitive "Error: Cantidad de operandos incorrecta")
                               (- (car args) (cadr args))))))
          
          (mult-prim () (let((args (to-number args)))
                          (if (equal?  (verificarTipoNumero (car args) (cdr args)) #f) (eopl:error 'apply-primitive "Error: Numeros de diferente tipo")
                          (if (null? (cdr args))  (eopl:error 'apply-primitive "Error Cantidad de operandos incorrecta")
                                 (operar * args 1)))))
           
          (div-prim ()  (let((args (to-number  args)))
                          (if (equal?  (verificarTipoNumero (car args) (cdr args)) #f) (eopl:error 'apply-primitive "Error: Numeros de diferente tipo")
                          (if (= 0 (cadr args)) (eopl:error 'apply-primitive "Error Division por 0")
                                 (/ (car args) (cadr args))))))
          
          (menor-prim() (let((args (to-number  args))) (if (equal? (< (car args) (cadr args)) #t) "true" "false")))
          (meneq-prim() (let((args (to-number  args))) (if (equal? (<= (car args) (cadr args)) #t) "true" "false")))
          (mayor-prim() (let((args (to-number  args))) (if (equal? (> (car args) (cadr args)) #t) "true" "false")))
          (mayig-prim() (let((args (to-number  args))) (if (equal? (>= (car args) (cadr args)) #t) "true" "false")))
          (igual-prim() (let((args (to-number  args))) (if (equal? (equal? (car args) (cadr args)) #t) "true" "false")))
          (orelse-prim() (let((args (to-number  args)))
                           (if (and (or(equal? (car args) #t)(equal? (car args) #f)) (or(equal? (cadr args) #t)(equals? (cadr args) #f)))
                             (or (car args) (cadr args)))))
          (andthen-prim() (let((args (to-number prim args)))(and (car args) (cadr args))))
          (unif-prim() ())
          (isfree-prim() (isFree2?(get-serial(car args))))
          (isdet-prim() (not(isFree2?(get-serial(car args)))))
    
          (newcell-prim() 
                       (create-cell args))
          
          (valcell-prim() 
                       (let((val
                       (car (apply-store(get-last-ref2 (get-serial (car args)))))))
                         (if (celda? val)
                             (if (list? (get-valor val))
                             (car(get-valor val))
                             (get-valor val))
                             (eopl:error 'apply-primitive "No es una variable de tipo celda ~s" args ))))
          
          (iscell-prim() 
                      (let((val
                       (car (apply-store(get-last-ref2 (get-serial (car args)))))))
                         (if (celda? val)
                             #t
                             #f)))
          
          
          (setcell-prim()
                       (let((val
                       (get-last-ref2 (get-serial (car args)))))
                         (let((cel(create-cell (car(cdr args)))))
                           (set-store val cel)
                         )))
          
          
          (newport-prim() 
                       (let ((port
                       (create-port args)))
                         (let ((var
                                (create-var (get-serial (car args)) (get-serial (car args)))))
                           (let ((varPort
                  (create-var  (length(vector-ref init-store 0))port )))
                         (update-store varPort)
                         (if (variable? (car args))
                          (set-store (get-serial (car args)) port))
                      (create-port (get-serial varPort))))))
          
           (valport-prim() 
                       (let((val
                       (car (apply-store(get-last-ref2 (get-serial (car args)))))))
                         (if (puerto? val)
                             (if (list? (get-valor val))
                             (car(get-valor val))
                             (get-valor val))
                             (eopl:error 'apply-primitive "No es una variable de tipo celda ~s" args ))))
          
          
          
          (isport-prim() 
                      (let((val
                       (car (apply-store(get-last-ref2 (get-serial (car args)))))))
                         (if (puerto? val)
                             #t
                             #f)))
          
          
          (sendP-prim()
                     (let((val
                       (car (apply-store(get-last-ref2 (get-serial (car args)))))))
                         (if (puerto? val)
                             (let ((pos
                                    (get-last-ref2 (get-serial (car args)))))
                               (let ((vecP (make-vector 1)))
                                   (vector-set! vecP 0 (car(cdr args))) 
                               (if (vector? (get-valor val))
                                   
                                   ;(eopl:error 'apply-primitive "noooooooooo ~s"  444)
                                    (let ((vals(vector-append (get-valor val) vecP)))
                                      (let ((portVec
                       (create-port vals)))
                                 
                             (set-store pos portVec)))
                                      ;vals)
                                      ;(if (puerto? val)
                                   ;(eopl:error 'apply-primitive "noooooooooo ~s"  (get-valor val)) 
                             
;                               (if (vector? vecP)
;                                   (eopl:error 'apply-primitive "noooooooooo ~s"  44444)
                               (let ((portVec
                       (create-port vecP)))
                                 
                             (set-store pos portVec))))))))
                             
                             ;(eopl:error 'apply-primitive "noooooooooo ~s"  (get-last-ref2 (get-serial (car args))))
                             ;(eopl:error 'apply-primitive "noooooooooo ~s"  (car(cdr args)))
                     
          
      )))


(define vector-append
  (lambda vecs
    (let* ((len (apply + (map vector-length vecs)))
           (result (make-vector len)))
      (let loop ((result-index 0)
                 (source-index 0)
                 (rest-of-vecs vecs))
        (cond ((null? rest-of-vecs) result)
              ((= source-index (vector-length (car rest-of-vecs)))
               (loop result-index 0 (cdr rest-of-vecs)))
              (else
               (vector-set! result result-index
                            (vector-ref (car rest-of-vecs) source-index))
               (loop (+ result-index 1) (+ source-index 1) rest-of-vecs)))))))
 
    
;*******************************************************************************************
 ;***************************FUNCIONES AUX APPLY PRIMITIVE***********************************

;Funcion que opera un conjunto de numeros dependiendo del signo
(define operar
     (lambda (pred lista n)
       (if (null? lista) n
           (if (= n 0 ) (operar pred (cdr lista) (pred (car lista) n))
           (operar pred (cdr lista) (pred n (car lista) ))))))


;Funcion para convertir un symbolo a numero
;(substring "~5" 0 1) retorna ~

(define parse-to-number
  (lambda (args)
    (if (null? args) ()
        (if (equal? (substring  args 0 1) "~") 
            (* -1 (string->number(substring args 1)))
            (string->number args)
            ))))

(define unparse
  (lambda (args)
    (if (>= args 0)
         args
        (string->symbol (string-append "~" (number->string(* -1 args)))))))
        

;Funcion que cambia a la notacion del interprete los numeros
(define evaluarResultado
  (lambda (dato)
    (if (< dato 0)  (string->symbol(string-append "~" (number->string(* -1 dato))))
        dato)))



;Cambio en verificar tipo numero, uso de funcion exact de schme, si retorna #t es pq todos los numeros son del mismo tipo
(define verificarTipoNumero 
  (lambda(primero resto)
    (if (null? resto) #t
        (if (and (exact? primero) (exact? (car resto))) (verificarTipoNumero primero (cdr resto))
            (if (and (not(exact? primero)) (not(exact? (car resto)))) (verificarTipoNumero primero (cdr resto))
                #f)))))
  

;funcion que transforma todos los valores d una lista a numeros ya que en apply-primitive es posible hacer +{X Y} donde X y Y son variables
;prueba
;(define D (create-var 0 4))
;(define E (create-var 1 5))
;(define listica (list D 412 E 76))
; (to-number 'a listica)
 
(define to-number
  (lambda(args)
    (if (null? args) ()
       (if (number? (car args)) (cons (car args) (to-number (cdr args)))
           (if (variable? (car args)) (cons (car(apply-store(get-serial (car args)))) (to-number (cdr args))))))))
  

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
;(define true-value?a
;  (lambda (x)
;    (not (zero? x))))
 
 (define true-value?
    (lambda (exp)
      (cond ((eqv? exp 'true)#t)
            ((eqv? exp 'false) #f)
            ((= exp 0) #f)
            (else #t))))

;*******************************************************************************************
 ;****************************************PROCEDIMIENTOS***********************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body cuerpo?)
   (env environment?)))
;Funciones

(define-datatype funval funval?
  (clase
   (ids (list-of symbol?))
   (body cuerpo?)
   (env environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (if (procval? proc)
        (cases procval proc
          (closure (ids body env)
                   (eval-cuerpo body (extend-env ids args env))))
        (if (funval? proc)
            (cases funval proc
          (clase (ids body env)
                   (eval-cuerpo body (extend-env ids args env))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype target target?
    (direct-target (expval expval?))
    (indirect-target (ref ref-to-direct-target?)))

(define expval?
    (lambda (x)
      (or (number? x) (procval? x) (boolean? x) (list? x) (symbol? x))))



(define scheme-value? (lambda (v) #t))


;*******************************************************************************************
;**********************************FUNCIONES AMBIENTES**************************************
;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env) 
    (extended-env-record syms (list->vector vals) env))) 


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (vector-ref vals pos)
                                 (apply-env env sym)))))))

(define apply-env-env
  (lambda (env sym newSerial)
    (cases environment env
      (empty-env-record ()
                        #f) ;(eopl:error 'apply-env-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (vector-set! vals pos newSerial)
                                 (apply-env-env env sym newSerial)))))))
  
  
;*******************************************************************************************
;**********************************FUNCIONES STORE******************************************
;funcion que actualiza un store, agregando un valor al final  
(define update-store
  (lambda (valor)
    (vector-set! init-store 0 (append (vector-ref init-store 0)(list valor)))))

;funcion que actualiza un store, cambiando un elemento de él
(define set-store
  (lambda (pos val)
    (vector-set! init-store 0 (setElement (vector-ref init-store 0) pos (create-var pos val)))))


;funcion que actualiza un store y devuelve el serial dl ultimo elemento 
(define update-store2
  (lambda (valor)
    (vector-set! init-store 0 (append (vector-ref init-store 0)(list valor)))
    (list-ref (vector-ref init-store 0) (- (length (vector-ref init-store 0)) 1))
    ))


(define set-store-cell
  (lambda (pos val)
    (vector-set! init-store 0 (setElement (vector-ref init-store 0) pos (create-cell pos val)))))
               

;Funcion que busca un serial ene el store y devuelve su valor asociado
(define apply-store 
  (lambda (serial)
    (aux-apply-store serial (vector-ref init-store 0))))

(define aux-apply-store 
  (lambda(serial lista)
    (if (null? lista) (eopl:error 'apply-store "Posicion no encontrada" serial)
      (if (variable? (car lista))
          (cases variable (car lista)
            (a-variable (serialE valor)
                        (if (equal? serialE serial) 
                            valor
                            (aux-apply-store serial (cdr lista)))));ojo hasta aqui es de registros
          
;                            (if (celda? (car lista))
;                                (cases celda (car lista)
;                                  (a-cell (svalor)
;                                   (if (equal? serialC serial) 
;                            valor  
;                            (aux-apply-store serial (cdr lista))))))
                            ))))



;*******************************************************************************************
;**********************************FUNCIONES REGISTROS**************************************
;Funcion crear registro (al crear un reg los campos tambien pueden ser reg)
(define create-reg
  (lambda (etiq camp val env)
    (let ((vector-seriales (make-vector 1))
          (val-in-store(map (lambda (x) (let ((evalExp(eval-expression x env)))
                                          (if (registro? evalExp)
                                              (cases registro evalExp
                                                (reg-vacio (etiq) ())
                                                (reg-datos (etiq2 camp2 val2)(create-reg etiq2 camp2 (vector-ref val2 0) env))) 
                                                           (update-store2 (create-var  (length(vector-ref init-store 0))evalExp )))))
                            
                            val)))
      (let ((listSeriales (map (lambda (x) (get-serial x)) val-in-store)))
      (vector-set! vector-seriales 0 listSeriales)
      
      (let ((newReg (reg-datos etiq camp vector-seriales)))
        (update-store (create-var  (length(vector-ref init-store 0)) newReg))
        (list-ref (vector-ref init-store 0) (- (length (vector-ref init-store 0)) 1))
    )))))


       

;FUNCION: asignar un valor a un campo de un registro
(define asig-val-cam-reg
  (lambda (reg cam val)
    (cases registro reg
      (reg-vacio (etiq)
                 (eopl:error 'asignar-val-registro
                             "Error: este registro no contiene campos"
                             val))
      (reg-datos (etiq campos vals)
                 (let ((pos (list-find-position cam campos)))
                   (if (number? pos)
                         (let ((valor(car(apply-store pos))))
                           (if (equal? valor '_)
                               (set-store pos val)
                               (eopl:error 'asignar-val-registro "Error: campo asignado" ))
                           (eopl:error 'asignar-val-registro
                            "Error: este registro no contiene el campo ~s" cam))))))))

   

;FUNCION: consultar valor de un campo en un registro
(define consultar-val-registro
  (lambda (reg cam)
    (cases registro reg
      (reg-vacio (etiq)
                 (eopl:error 'consultar-val-registro
                             "Error: este registro no contiene campos"
                             val))
      (reg-datos (etiq campos vals)
                 (let ((pos (list-find-position cam campos)))
                   (if (number? pos)
                       (list-ref (vector-ref vals 0) pos)
                       (eopl:error 'consultar-val-registro
                             "Error: este registro no contiene el campo ~s" campos)))))))


;Funcion que asigna una posicion a los elementos de un registro dentro del store
(define asignar-pos-en-store
  (lambda (lista)
    (asig-pos-env (length lista) (length(vector-ref init-store 0)) )))
 



;Funcion que verifica si dos registros se pueden unificar
(define eval-regs
  (lambda (reg1 reg2)
    (cases registro reg1
      (reg-vacio (etiq1) ())
      (reg-datos (etiq1 campos1 valores1)
                 (cases registro reg2
                   (reg-vacio (etiq2) () )
                   (reg-datos (etiq2 campos2 valores2) 
                              (if (and (eval-etiq etiq1 etiq2) (eval-camps campos1 campos2) (eval-camp-and-vals 0 campos1 valores1 campos2 valores2))
                                  #t #f
                                  )))))))

;Funcion q verifica q dos etiquetas de un reg sean iguales
(define eval-etiq
  (lambda (etiq1 etiq2)
    (equal? etiq1 etiq2)))

;Funcion que verifica que el los campos de un reg tengan el mismo largo
(define eval-camps
  (lambda (camp1 camp2)
    (equal?  (length camp1) (length camp2))))

;Funcion que determina si los campos del reg1 son iguales a los del reg2 (comparando internamente sus valores asociados)
(define eval-camp-and-vals
  (lambda (posCampo camp1 val1 camp2 val2)
    (if (equal? posCampo (length camp1)) #t
    (let ((campoActual (list-ref camp1 posCampo)))
    (let ((pos (rib-find-position campoActual camp2))) ; encuentre al campo1 en campo2
               (if (equal? pos #f) #f
                   (let ((serial-maximo-var1 (get-last-ref2 (list-ref (vector-ref val1 0) (rib-find-position campoActual camp1))))
                         (serial-maximo-var2 (get-last-ref2 (list-ref  (vector-ref val2 0) pos))))
                     (if (or (equal? (car (apply-store serial-maximo-var1)) (car (apply-store serial-maximo-var2))) 
                             (or (equal? (car (apply-store serial-maximo-var1)) '_) (equal? (car (apply-store serial-maximo-var2)) '_)))
                         (eval-camp-and-vals (+ posCampo 1) camp1 val1 camp2 val2)
                         (if (and (registro? (car (apply-store serial-maximo-var1))) (registro? (car (apply-store serial-maximo-var2))) )
                             (eval-regs (car (apply-store serial-maximo-var1))  (car (apply-store serial-maximo-var2)))
                         #f
                    )))))))))
    

;Funcion que unifica registros
(define unif-regs
  (lambda (reg1 reg2)
    (cases registro reg1
      (reg-vacio (etiq1) ())
      (reg-datos (etiq1 campos1 valores1)
                 (cases registro reg2
                   (reg-vacio (etiq2) () )
                   (reg-datos (etiq2 campos2 valores2) 
                              (unif-camps-reg 0 campos1 valores1 campos2 valores2))
                                  )))))
    
;Funcion q unifica los campos de un registro (ojoooo: antes estaba -> (set-store posCampo2 (create-var posCamp1 campoActual))
(define unif-camps-reg
  (lambda (posCampo camp1 val1 camp2 val2)
    (if (equal? posCampo (length camp1)) #t
    (let ((campoActual (list-ref camp1 posCampo)))
    (let ((pos (rib-find-position campoActual camp2))) ; encuentre al campo1 en campo2
               (if (equal? pos #f) #f
                   (let ((posCamp1 (list-ref (vector-ref val1 0) (rib-find-position campoActual camp1)))
                         (posCamp2 (list-ref  (vector-ref val2 0) pos)))
                   (let ((serial-maximo-var1 (get-last-ref2 posCamp1))
                         (serial-maximo-var2 (get-last-ref2 posCamp2)))
                     (if (equal? serial-maximo-var1 serial-maximo-var2)
                           (unif-camps-reg (+ posCampo 1) camp1 val1 camp2 val2)
                           (if (and (equal? (car (apply-store serial-maximo-var1)) '_) (equal? (car (apply-store serial-maximo-var2)) '_))
                               (begin
                                 (set-store serial-maximo-var2 (create-var posCamp1 campoActual))
                                 (unif-camps-reg (+ posCampo 1) camp1 val1 camp2 val2))
                               (if  (and (equal? (car (apply-store serial-maximo-var1)) '_) (not(equal? (car (apply-store serial-maximo-var2)) '_)))
                                    (begin
                                      (set-store serial-maximo-var1 (create-var posCamp2 campoActual))
                                      (unif-camps-reg (+ posCampo 1) camp1 val1 camp2 val2))
                                    (if  (and (not (equal? (car (apply-store serial-maximo-var1)) '_)) (equal? (car (apply-store serial-maximo-var2)) '_))
                                         (begin
                                           (set-store serial-maximo-var2 (create-var posCamp1 campoActual))  
                                           (unif-camps-reg (+ posCampo 1) camp1 val1 camp2 val2))
                                         (if  (and (registro? (car (apply-store serial-maximo-var1))) (registro? (car (apply-store serial-maximo-var2))))
                                              (begin
                                                (set-store serial-maximo-var1 (create-var posCamp2 campoActual))
                                                (unif-camps-reg (+ posCampo 1) camp1 val1 camp2 val2))
                                             
                   
                    )))))))))))))
      
      

;*******************************************************************************************
;**********************************FUNCIONES CELDAS**************************************
(define create-cell
  (lambda (valor)
    (let ((vector (make-vector 1)))
      (vector-set! vector 0 valor)
      (a-cell vector))))

;************celdas
  
(define create-port
  (lambda (valor)
    (let ((vector (make-vector 1)))
      (vector-set! vector 0 valor)
      (a-port vector))))




;****************************************************************************************
;FUNCIONES AUXILIARES GENERALES

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;cambia el elemento de una lista de acuerdo con su posicion (n)
(define setElement 
  (lambda (lista n elemento)
;    (if (= n 5)(eopl:error 'setElement "Varible ~s ya derminada" n))
    (if (= n 0) (cons elemento (cdr lista))
        (cons (car lista) (setElement (cdr lista) (- n 1) elemento)))))

;**************************************************************


  

(define miRegistro1 (reg-datos 'miRegistro '(campo1 campo2 campo3) #((_ 2 3))))
(define miRegistro2 (reg-datos 'miRegistro '(campo1 campo2 campo3) #((_ 2 3))))



;(asig-val-cam-reg miRegistro 'campo1 2)
;(update-store (create-var (length(vector-ref init-store 0)) miRegistro))
;init-store
;(asignar-pos-en-store '(1 2 3))
