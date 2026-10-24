#lang eopl


;;definicion BNF para las expresiones del lenguaje:
;;
;; <programa> :=  <expresion>
;;                un-programa (exp)
;; 
;; <expresion> := <numero>
;;             numero-lit (num)
;;
;;             := "\""<texto> "\""
;;             texto-lit (txt)
;;
;;             := <identificador>
;;             var-exp (id)
;;             
;;             := (<expresion> <primitiva-binaria> <expresion>)
;;             primapp-bin-exp (exp1 prim-binaria exp2)
;;             
;;             := <primitiva-unaria> (<expresion>)
;;             primapp-un-exp (prim-unaria exp) 
;;             
;; <primitiva-binaria> :=  + (primitiva-suma)
;;
;;                     :=  ~ (primitiva-resta)
;;
;;                     :=  / (primitiva-div)
;;
;;                     :=  * (primitiva-multi)
;;
;;                     :=  concat (primitiva-concat)
;;
;;                     := > (primitiva-mayor)
;;
;;                     := < (primitiva-menor)
;;
;;                     := >= (primitiva-mayor-igual)
;;
;;                     := <= (primitiva-menor-igual)
;;
;;                     := != (primitiva-diferente)
;;
;;                     := == (primitiva-comparador-igual)
;;
;; <primitiva-unaria> :=  longitud (primitiva-longitud)
;;
;;                    :=  add1 (primitiva-add1)
;;
;;                    :=  sub1 (primitiva-sub1)
;;
;;                    := neg (primitiva-negacion-booleana)

;Especificación Léxica

(define interprete-lexico-simple
  '(
    (espacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
    (texto (letter (arbno (or letter digit "_"))) string)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

;Especificación sintáctica (gramática)

(define interprete-gramatical-simple
  '(
    (programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ( "\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ( "(" expresion primitiva-binaria expresion ")")  primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion  "}" "sino" "{" expresion "}")
                condicional-exp)
    (expresion ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    )
  )

;datatypes automaticos:

(sllgen:make-define-datatypes interprete-lexico-simple interprete-gramatical-simple)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes interprete-lexico-simple interprete-gramatical-simple)))

;FrontEnd (analisis léxico y sintáctico)

(define scan&parse
  (sllgen:make-string-parser interprete-lexico-simple interprete-gramatical-simple))

;Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner interprete-lexico-simple interprete-gramatical-simple))

;Interpretador

(define interpretador
  (sllgen:make-rep-loop "-->"
                        (lambda (pgm)(evaluar-programa pgm))
                        (sllgen:make-stream-parser
                         interprete-lexico-simple
                         interprete-gramatical-simple)))


;El Interprete

;evaluar-programa: funcion que evalua un programa teniendo en cuenta un ambiente

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                   (evaluar-expresion cuerpo (init-env))))))


;evaluar-expresion: funcion que evalua la expresion en el ambiente dado

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (dato) dato)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id env))
      (primapp-bin-exp (rand1 prim rand2)
                       (aplicar-primitiva-binaria(evaluar-expresion rand1 env) prim (evaluar-expresion rand2 env)))
      (primapp-un-exp (prim exp)
                      (aplicar-primitiva-unaria prim (evaluar-expresion exp env)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (evaluar-expresion test-exp env))
                           (evaluar-expresion true-exp env)
                           (evaluar-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                         (evaluar-expresion cuerpo ()))
      )))

;apicar-primitiva-binaria: funcion que aplica la primitiva binaria correspondiente a un par de expresiones

(define aplicar-primitiva-binaria
  (lambda (arg1 prim arg2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
      (primitiva-mayor () (if (> arg1 arg2) 1 0))
      (primitiva-menor () (if (< arg1 arg2) 1 0))
      (primitiva-mayor-igual () (if (>=  arg1 arg2) 1 0))
      (primitiva-menor-igual () (if (<= arg1 arg2) 1 0))
      (primitiva-diferente () (if (not (equal? arg1 arg2)) 1 0))
      (primitiva-comparador-igual () (if (equal? arg1 arg2) 1 0))
      )))


;aplicar-primitiva-unaria: funcion que aplica la primitiva unaria correspondiente a una expresion

(define aplicar-primitiva-unaria
  (lambda (prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length arg))
      (primitiva-add1 () (+ 1 arg))
      (primitiva-sub1 () (- arg 1))
      (primitiva-negacion-booleana () (not arg))
      )))



;Funciones de ambientes
(define valor-verdad?
  (lambda (cond)
    (not (zero? cond))))

;Ambiente Inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))
    

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (simbolos (list-of symbol?))
                       (valores (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))


;función que crea un ambiente vacío

(define empty-env  
  (lambda ()
    (empty-env-record))) 


;función que crea un ambiente extendido

(define extend-env
  (lambda (simbolos vals env)
    (extended-env-record simbolos vals env)))


;buscar-variable: funcion que busca una variable en un ambiente

(define buscar-variable
  (lambda (identificador env)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "Error, la variable" identificador "no existe"))
      (extended-env-record (identificadores valores env)
                           (let ((posicion (encontrar-posicion-lista identificador identificadores)))
                             (if (number? posicion)
                                 (list-ref valores posicion)
                                 (buscar-variable identificador env))))
      )
    )
  )



;funciones auxiliares para hallar la posición de un símbolo (identificador) en la
;lista de símbolos (identificadores) de un ambiente

(define encontrar-posicion-lista
  (lambda (simbolo lista)
    (indice-lista (lambda (simbolo2) (eqv? simbolo2 simbolo)) lista)
    )
  )

(define indice-lista
  (lambda (predicado lista)
    (cond
      ((null? lista) #f)
      ((predicado (car lista)) 0)
      (else (let ((indice-lista-acumulado (indice-lista predicado (cdr lista))))
              (if (number? indice-lista-acumulado)
                (+ 1 indice-lista-acumulado)
                #f)
              )
            )
      )
    )
  )
