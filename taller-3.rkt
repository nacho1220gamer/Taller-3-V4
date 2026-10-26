#lang eopl

;;
;;Ignacio Henao Henao 2438110
;;Juan Fernando Marmolejo Valencia 2437661
;;Samuel  Saldaña  Giraldo 2437631 
;;Alejandro Cardona Arellano 2439107
;;https://github.com/nacho1220gamer/Taller-3-V4.git


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
;;             := "evaluar" <expresion> "(" {<expresion>}*(",") ")" "finEval"
;;             app-exp (exp exps)
;;
;;             := "recursivo" "(" {<identificador> "(" {<identificador>}*(",") ")" "=" <expresion> ";"}* ")" "{" <expresion> "}"
;;             recursivo-exp (ids lista-ID exps cuerpo)
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
    (texto ((or letter "_") (arbno (or letter digit "_" ":"))) string)
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
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}")
               procedimiento-exp)
    (expresion ("recursivo" "("
                (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion ";")
                ")" "{" expresion "}")
               recursivo-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)
    
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
                   (evaluar-expresion cuerpo (ambiente-inicial))))))


;evaluar-expresion: funcion que evalua la expresion en el ambiente dado

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (dato) dato)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id amb))
      (primapp-bin-exp (rand1 prim rand2)
                       (aplicar-primitiva-binaria(evaluar-expresion rand1 amb) prim (evaluar-expresion rand2 amb)))
      (primapp-un-exp (prim exp)
                      (aplicar-primitiva-unaria prim (evaluar-expresion exp amb)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (evaluar-expresion test-exp amb))
                           (evaluar-expresion true-exp amb)
                           (evaluar-expresion false-exp amb)))
      (variableLocal-exp (ids exps cuerpo)
                         (evaluar-expresion cuerpo (ambiente-extendido ids (evaluar-lista-expresiones exps amb) amb)))
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo amb))
         
      ;;Punto 7
            (app-exp (exp exps)
               (cases procVal (evaluar-expresion exp amb)
                 (cerradura (lista-ID cuerpo amb-cerradura)
                            (evaluar-expresion cuerpo
                                               (ambiente-extendido lista-ID
                                                                   (evaluar-lista-expresiones exps amb)
                                                                   amb-cerradura)))))
      ;;Punto 8
      (recursivo-exp (ids lista-ID exps cuerpo)
                     (evaluar-expresion cuerpo (ambiente-extendido-recursivo ids lista-ID exps amb)))
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


;valor-verdad
(define valor-verdad?
  (lambda (cond)
    (not (zero? cond))))

;Funciones de ambientes

;Ambiente Inicial

(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (ambiente-vacio))))
    

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (registro-ambiente-vacio)
  (registro-ambiente-extendido (simbolos (list-of symbol?))
                       (valores (list-of scheme-value?))
                       (amb ambiente?))
  (registro-ambiente-extendido-recursivo (simbolos (list-of symbol?))
                           (lista-ID (list-of (list-of symbol?)))
                           (exps (list-of expresion?))
                           (amb ambiente?)))

;definición del tipo de dato procVal (closure/cerradura)
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

(define scheme-value? (lambda (v) #t))


;función que crea un ambiente vacío

(define ambiente-vacio  
  (lambda ()
    (registro-ambiente-vacio))) 


;función que crea un ambiente extendido

(define ambiente-extendido
  (lambda (simbolos vals amb)
    (registro-ambiente-extendido simbolos vals amb)))

;;función que crea un ambiente extendido para procedimeintos que hagan uso de recursividad

(define ambiente-extendido-recursivo
  (lambda (simbolos lista-ID exps amb)
    (registro-ambiente-extendido-recursivo simbolos lista-ID exps amb)))


;buscar-variable: funcion que busca una variable en un ambiente


(define buscar-variable
  (lambda (identificador amb)
    (cases ambiente amb
      (registro-ambiente-vacio ()
        (eopl:error 'buscar-variable "Error, la variable ~s no existe" identificador))
      (registro-ambiente-extendido (identificadores valores amb-anterior)
        (let ((posicion (encontrar-posicion-lista identificador identificadores)))
          (if (number? posicion)
              (list-ref valores posicion)
              (buscar-variable identificador amb-anterior))))
      (registro-ambiente-extendido-recursivo (simbolos lista-ID exps amb-anterior)
        (let ((posicion (encontrar-posicion-lista identificador simbolos)))
          (if (number? posicion)
              (cerradura (list-ref lista-ID posicion)
                         (list-ref exps posicion)
                         amb)
              (buscar-variable identificador amb-anterior))))
      )))


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

;evaluar-lista-expresiones: funcion auxiliar que evalua cada expresion de una lista en el ambiente dado
;y retorna la lista de los valores resultantes

(define evaluar-lista-expresiones
  (lambda (lista-exps amb)
    (cond
      ((null? lista-exps) '())
      (else (cons (evaluar-expresion (car lista-exps) amb)
                  (evaluar-lista-expresiones (cdr lista-exps) amb))))))

;; Utilización del lenguaje de programación
;; -----------------------------------------------------------------------------
;; Ejercicio a) @sumarDigitos(n)
;; Descripción: Suma los dígitos de un número entero positivo.
;; Ejemplo: evaluar @sumarDigitos(147) finEval -> Retorna 12
;; -----------------------------------------------------------------------------
#|
recursivo (
  @div10(@x) = 
    Si (@x < 10) {
      0
    } sino {
      (1 + evaluar @div10((@x ~ 10)) finEval)
    };
  @sumarDigitos(@n) = 
    Si (@n == 0) {
      0
    } sino {
      declarar (
        @div = evaluar @div10(@n) finEval;
      ) {
        declarar (
          @ultimo = (@n ~ (10 * @div));
        ) {
          (@ultimo + evaluar @sumarDigitos(@div) finEval)
        }
      }
    };
) {
  evaluar @sumarDigitos(147) finEval
}
|#


;; -----------------------------------------------------------------------------
;; Ejercicio b)@sumaNaturales(n)
;; Descripción: Calcula la suma de los primeros n enteros positivos.
;; Ejemplos: evaluar @sumaNaturales(5) finEval -> Retorna 15
;;           evaluar @sumaNaturales(10) finEval -> Retorna 55
;; -----------------------------------------------------------------------------
#|
recursivo (
  @sumaNaturales(@n) = 
    Si (@n == 0) {
      0
    } sino {
      (@n + evaluar @sumaNaturales(sub1(@n)) finEval)
    };
) {
  evaluar @sumaNaturales(5) finEval
}
Ejemplo-alt: evaluar @sumaNaturales(10) finEval
|#


;; -----------------------------------------------------------------------------
;; Ejercicio c)@multiplicar(a, b)
;; Descripción: Calcula el producto de dos enteros no negativos mediante sumas 
;;              sucesivas.
;; Ejemplo: evaluar @multiplicar(4, 3) finEval -> Retorna 12
;; -----------------------------------------------------------------------------
#|
recursivo (
  @multiplicar(@a, @b) = 
    Si (@a == 0) {
      0
    } sino {
      (@b + evaluar @multiplicar(sub1(@a), @b) finEval)
    };
) {
  evaluar @multiplicar(4,3) finEval
}
|#


;; -----------------------------------------------------------------------------
;; Ejercicio d)@sumaParesRango(a, b)
;; Descripción: Calcula la sume los números pares en un rango de valores positivos 
;; [a,b] donde a < b
;; Ejemplo: evaluar @sumaParesRango (2, 7) finEval -> Retorna 12
;; -----------------------------------------------------------------------------
#|
recursivo (
  @CuantasVecesCabe2(@x) = 
    Si (@x < 2) {
      0
    } sino {
      (1 + evaluar @CuantasVecesCabe2((@x ~ 2)) finEval)
    };
  @residuo2(@x) =
    (@x ~ (2 * evaluar @CuantasVecesCabe2(@x) finEval))
   ;
  @sumaParesRango(@a, @b) = 
  Si (@a < @b) {
     Si (evaluar @residuo2(@a) finEval == 0){
       (@a + evaluar @sumaParesRango(add1(@a), @b) finEval)
     } sino  {
       (0 + evaluar @sumaParesRango(add1(@a), @b) finEval)
        }
  }
  sino {0};
) {
evaluar @sumaParesRango(2,7) finEval
}

|#

;; -----------------------------------------------------------------------------
;; Ejercicio e)@integrantes @saludar @decorate
;; Descripción: @integrantes Retorna un string con los nombres ed los integrantes del grupo
;;              @saludar Recibe un procedimiento y retorna otro que le antepone el texto
;;              "Hola:" al resultado del procedimiento que recibió
;;              @decorate Procedimiento decorado
;; Ejemplo: evaluar @decorate () finEval -> Retorna "Hola:Ignacio_Samuel_Alejandro_y_Fernando"
;; -----------------------------------------------------------------------------
#|

declarar (
  @integrantes = procedimiento () { "Ignacio_Samuel_Alejandro_y_Fernando" };
  @saludar = procedimiento (@f) {
    procedimiento () {
      ("Hola:" concat evaluar  @f() finEval)
    }
  };
) {
  declarar (
     @decorate = evaluar @saludar (@integrantes) finEval;
) {
  evaluar @decorate () finEval
  }
}
|#
;-------------------------------------------------------------------------------
;; Ejercicio f) @integrantes @saludar @decorate (con mensaje adicional)
;; Descripción: @integrantes retorna un string con los nombres de los integrantes.
;;              @saludar recibe un procedimiento @f y retorna otro procedimiento que
;;              ahora si recibe un parámetro @msg: antepone "Hola:" al resultado de
;;              invocar @f y agrega @msg al final del string.
;;              @decorate es el procedimiento decorado resultante.
;; Ejemplo: evaluar @decorate ("_EstudiantesFLP") finEval-> Retorna "Hola:Ignacio_Samuel_Alejandro_y_Fernando_EstudiantesFLP"
;-------------------------------------------------------------------------------
#|

declarar (
  @integrantes = procedimiento () { "Ignacio_Samuel_Alejandro_y_Fernando" };
  @saludar = procedimiento (@f) {
    procedimiento (@msg){
      (("Hola:" concat evaluar @f() finEval) concat @msg)
    }
  };
) {
  declarar (
   @decorate = evaluar @saludar (@integrantes) finEval;
  ){
   evaluar @decorate ("_EstudiantesFLP")finEval
  }
}


|#
