;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Proyecto) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "gui.ss" "teachpack" "htdp")))))
;Juan David Ospina Vasquez 0835163  
;Carolina
;Roberto Ceballos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;111111

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= 
;;  <primitive>     ::= 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define hola)

(define scanner-spec-simple-interpreter
'((white-sp(whitespace) skip)
  (comment("%" (arbno (not #\newline))) skip) 
  (variable ((or "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "Ñ" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z") 
               (arbno (or letter digit "?")) ) symbol)
  ;(atomo ((or "a" "b" "c" "ch" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "ñ" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  
  (numberE(digit (arbno digit)) number)
  (negNumberE ("~" (arbno digit)) symbol)
  (numberF (digit (arbno digit) "." digit (arbno digit))number)
  (negNumberF ("~" digit (arbno digit) "." digit (arbno digit))symbol)
  ))


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (program (expression) a-program)
    
    (cuerpo (expression (arbno expression)) cuerpoc)
    
    (expression (variable) var-exp)
    (expression (entero) entero-exp)
    (expression (flotante) flotante-exp)
    
    (expression ("local" "{" (arbno variable)"}" "in" cuerpo "end") local-exp)
                              
    (expression (primitive "{" (arbno expression)"}") primapp-exp) 
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("proc" "(" (arbno variable) ")" expression)proc-exp)
    (expression ( "{" expression (arbno expression) "}") app-exp)
    (expression ("set" variable "=" expression)set-exp)
    
    
    ;BOOLEANOS
    (expression (boolean) bool-exp)
    (boolean ("true") true-exp)
    (boolean ("false") false-exp)
    
    ;NUMEROS
    (entero (numberE) posEntero)
    (entero(negNumberE) negEntero)
    
    (flotante(numberF) posFlotante)
    (flotante(negNumberF) negFlotante)
    
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
    
    ;Logicas
    (primitive ("orelse") orelse-prim)
    (primitive ("andthen") andthen-prim)
    
    ;Unificacion
    (primitive ("=") unif-prim)
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
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))


(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))


;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (entero-exp (ent) 
                 (cases entero ent
                 (posEntero (num) num)
                 (negEntero (num)  num)))
      
      (flotante-exp (flt) 
                 (cases flotante flt
                 (posFlotante (num) num)
                 (negFlotante (num)  num)))
      
      (var-exp (id) id)
      
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      
      (bool-exp (exp)
                (cases boolean exp
                  (true-exp () #t)
                  (false-exp () #f)))
      
      (local-exp (vars body)
                 (eval-cuerpo body env))
      )))

;-------------------------------------------------------------------------------------------------------------------
;FUNCIONES AUXILIARES EVAL-EXPRESSION
(define eval-cuerpo
  (lambda(body env)
    (cases cuerpo body
      (cuerpoc (primero resto)
               (if (null? resto) (eval-expression primero env)
                   (begin
                     (eval-expression primero env)
                    (eval-cuerpo (cuerpoc (car resto) (cdr resto)) env)
   ))))))
    
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))
;-------------------------------------------------------------------------------------------------------------------

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (let ((args (convertir args)) )
      (let( (valores (verificarTipoNumero (car args) (cdr args))))
        (cases primitive prim
          (sum-prim () (if (null? (cdr args)) (eopl:error 'apply-primitive "Error: Minimo requiere dos operandos")
                       (if  (not valores) (eopl:error 'apply-primitive "Error: Diferentes tipos de numeros")
                        (evaluarResultado(operar + args 0)))))
      
          (sub-prim () (if (or (null? (cdr args))(not (null? (caddr args)))) (eopl:error 'apply-primitive "Error: Cantidad de operandos incorrecta")
                           (if  (not valores) (eopl:error 'apply-primitive "Error: Diferentes tipos de numeros")
                                (evaluarResultado(- (car args) (cadr args))))))
          
          (mult-prim () (if (null? (cdr args))  (eopl:error 'apply-primitive "Error Cantidad de operandos incorrecta")
                            (if  (not valores) (eopl:error 'apply-primitive "Error: Diferentes tipos de numeros")
                                 (evaluarResultado(operar * args 1)))))
          
          (div-prim ()  (if (= 0 (cadr args)) (eopl:error 'apply-primitive "Error Division por 0")
                            (if  (not valores) (eopl:error 'apply-primitive "Error: Diferentes tipos de numeros")
                                 (evaluarResultado(/ (car args) (cadr args))))))
          
          (menor-prim() (< (car args) (cadr args)))
          (meneq-prim() (<= (car args) (cadr args)))
          (mayor-prim() (> (car args) (cadr args)))
          (mayig-prim() (>= (car args) (cadr args)))
          (igual-prim() (car args))
          (orelse-prim() (if (and (or(equals? (car args) #t)(equals? (car args) #f))(or(equals? (cadr args) #t)(equals? (cadr args) #f)))
                             (or (car args) (cadr args))))
          (andthen-prim() (and (car args) (cadr args)))
          (unif-prim() ())
          
   
      )))))

;-------------------------------------------------------------------------------------------------------------------
;FUNCIONES AUX APPLY PRIMITIVE

;Funcion que opera un conjunto de numeros dependiendo del signo
(define operar
     (lambda (pred lista n)
       (if (null? lista) n
           (if (= n 0 ) (operar pred (cdr lista) (pred (car lista) n))
           (operar pred (cdr lista) (pred n (car lista) ))))))


;Funcion que convierte un numero negativo en notacion real
(define convertir
  (lambda (args)
    (if (null? args) ()
    (if (symbol? (car args)) (cons (* -1 (string->number(substring (symbol->string(car args)) 1))) (convertir (cdr args)))
        (cons (car args) (convertir (cdr args)))))))


;Funcion que cambia a la notacion del interprete los numeros
(define evaluarResultado
  (lambda (dato)
    (if (< dato 0)  (string->symbol(string-append "~" (number->string(* -1 dato))))
        dato)))


(define verificarTipoNumero
  (lambda  (primero lista)
    (if (null? lista) #t
    (if (and (= (- primero (truncate primero))0) (=(- (car lista) (truncate (car lista)) )0))(verificarTipoNumero primero (cdr lista))  ;si es = a cero es entero
      (if (and (not(= (- primero (truncate primero))0)) (not(=(- (car lista) (truncate (car lista)) )0))) (verificarTipoNumero primero (cdr lista))
           #f)))))





;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
 (define true-value?
    (lambda (exp)
      (cond ((eqv? exp 'true)#t)
            ((eqv? exp 'false) #f)
            ((= exp 0) #f)
            (else #t))))
;-------------------------------------------------------------------------------------------------------------------

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

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

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error ’deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

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

;******************************************************************************************

(define read-eval-print
  (sllgen:make-rep-loop "-->" eval-program
                        (sllgen:make-stream-parser scanner-spec-simple-interpreter
                                                   grammar-simple-interpreter)))

(read-eval-print)



;(interpretador)


           
                       


