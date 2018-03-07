;; Autores:
;;  Aitor Arnaiz del Val
;;  Alejandro Cabana Suárez

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dot-product-rec (u v)
;;; Calcula el producto escalar de dos vectores de forma recursiva
;;;
;;; INPUT: u: vector, representado como una lista
;;; v: vector, representado como una lista
;;;
;;; OUTPUT: producto escalar de u y v ( u·v )
;;;
(defun dot-product-rec (u v)
  (if (or (null u) (null v))
      0
    (+ (* (first u) (first v))
       (dot-product-rec (rest u) (rest v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-rec (v)
;;; Calcula la norma-2 (eucli­dea) de un vector de forma recursiva
;;;
;;; INPUT: v: vector, representado como una lista
;;;
;;; OUTPUT: norma-2 de v ( ||v|| )
;;;
(defun 2-norm-rec (v)
  (sqrt (dot-product-rec v v))) ; ||v||² = <v,v>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y
(defun sc-rec (x y)
  (if (or (null x)
          (null y)
          (every #'zerop x)
          (every #'zerop y))
      nil
    (/ (dot-product-rec x y)
       (* (2-norm-rec x)
          (2-norm-rec y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dot-product-mapcar (u v)
;;; Calcula el producto escalar de dos vectores usando mapcar
;;;
;;; INPUT: u: vector, representado como una lista
;;; v: vector, representado como una lista
;;;
;;; OUTPUT: producto escalar de u y v ( u·v )
;;;
(defun dot-product-mapcar (u v)
  (reduce #'+ (mapcar #'* u v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-mapcar (v)
;;; Calcula la norma-2 (eucli­dea) de un vector usando mapcar
;;;
;;; INPUT: v: vector, representado como una lista
;;;
;;; OUTPUT: norma-2 de v ( ||v|| )
;;;
(defun 2-norm-mapcar (v)
  (sqrt (dot-product-mapcar v v))) ; ||v||² = <v,v>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0...0) es NIL.
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;; OUTPUT: similitud coseno entre x e y
(defun sc-mapcar (x y)
  (if (or (null x)
          (null y)
          (every #'zerop x)
          (every #'zerop y))
      nil
    (/ (dot-product-mapcar x y)
       (* (2-norm-mapcar x)
          (2-norm-mapcar y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT: cat: vector que representa a una categoría, representado como una lista
;;; vs: vector de vectores
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud con respecto a la categoría es superior al
;;; nivel de confianza, ordenados
(defun sc-conf (cat vs conf)
  (mapcar #'rest                                                  ; quita el primer elemento (sc) de cada vector, dejando los originales
    (sort
     (remove nil                                                  ; elimina posibles nil
             (mapcar #'(lambda (z) (sc-conf-ind cat z conf)) vs)) ; añade al principio de cada vector su similitud con cat
     #'> :key #'first)))                                          ; ordena de mayor a menor segun el primer elemento

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf-ind (x v conf)
;;; Devuelve la similitud de un vector a una categoria seguida del vector,
;;; siempre que sea mayor que la confianza
;;;
;;; INPUT: x: vector, representado como una lista
;;; v: vector, representado como una lista
;;; conf: Nivel de confianza
;;;
;;; OUTPUT: Cons cuyo car es la similitud y cuyo cdr es el vector
;;;
(defun sc-conf-ind (cat v conf)
  (let ((sc (sc-rec cat v)))
    (if (or (null sc) (<= sc conf))	; si la similitud se ha podido hacer, y es mayor que conf
        nil
      (cons sc v))))			; mete la similitud como primer elemento, luego la quitaremos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorias.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
  (if (null texts)
      nil 										 ; caso base	
    (cons (reduce #'(lambda (x y)							 ; saca tupla (id, sc) con el maximo sc
                      (if (> (first (rest x)) (first (rest y))) ;como ahora son listas y no cons tengo que acceder al interior de la lista porque (num) no puede evaluarlo el operador >, pero si a su first
                          x
                        y))
                  (mapcar #'(lambda (cat)					     ; saca una lista con tuplas (id, sc) para cada categoria
                              (list (first cat)
                                    (funcall func (rest cat) (rest (first texts))))) ; aplica la funcion para sacar sc
                    cats))
          (sc-classifier cats (rest texts) func))))			             ; concateno este par con una lista de los pares siguientes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Banco de pruebas de sc-classifier
;;; 

(setf cats '((1 43 23 12) (2 33 54 24)))
(setf texts '((1 3 22 134) (2 43 26 58)))
(sc-classifier cats texts #'sc-rec)
;127'652 processor cycles
;0'000107 seconds
(sc-classifier cats texts #'sc-mapcar)
;75'488 processor cycles
;0'000033 seconds


(setf cats2 '((1 37 42 18 29 14) (2 24 38 93 16 2) (3 16 29 4 66 9) (4 86 77 9 6 3) (5 16 18 1 0 99) (6 14 18 23 2 76) (7 99 23 46 18 33) (8 13 14 15 70 90)))
(setf texts2 '((1 37 42 18 29 14) (2 45 66 3 14 2) (3 14 25 10 55 8) (4 8 45 6 77 21) (5 6 44 32 12 1) (6 66 4 33 52 6) (7 6 45 33 21 12) (8 4 5 66 77 88)))
;salida: ((1 1.0000001) (4 0.9632508) (3 0.99436706) (3 0.97984594) (2 0.8509972) (7 0.87027204) (1 0.85014236) (8 0.9203154))

(time(sc-classifier cats2 texts2 #'sc-rec))
;164'798 processor cycles
;0'000065 seconds
(time(sc-classifier cats2 texts2 #'sc-mapcar))
;299'454 processor cycles
;0'000117 seconds




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xnor (a b)
;;; Negacion del OR exclusivo, devuelve T si a y b tienen el mismo valor.
;;;
;;; INPUT: a: booleano
;;; b: booleano
;;;
;;; OUTPUT: T si a y b tienen el mismo valor, NIL en caso contrario
;;;
(defun xnor (a b)
  (or (and a b)
      (and (not a)
           (not b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finds a root of f between the points a and b using bisection.
;;
;; If f(a)f(b)>=0 there is no guarantee that there will be a root in the
;; interval, and the function will return NIL.
;; INPUT:
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;; OUTPUT: Root of the function, or NIL if no root is found
(defun bisect (f a b tol)
  (let* ((m (/ (+ a b) 2))           ; punto medio
         (fa (funcall f a))          ; f(a)
         (fb (funcall f b))          ; f(b)
         (fm (funcall f m))          ; f(m)
         (siga (> fa 0))             ; signo de f(a): T si positivo, NIL si negativo
         (sigb (> fb 0))             ; signo de f(b)
         (sigm (> fm 0)))            ; signo de f(m)
    (cond ((xnor siga sigb)          ; si f(a) y f(b) tienen el mismo signo
           nil)
          ((= fa 0)                 ; condicion de parada
           a)
          ((= fb 0)                 ; condicion de parada
           b)
          ((= fm 0)                 ; condicion de parada
           m)
          ((< (- b a) tol)          ; condicion de parada
           m)
          ((xnor sigm sigb)         ; si f(m) y f(b) tienen el mismo signo, f(a) y f(b) lo tienen distinto
           (bisect f a m tol))
          (t                        ; solo puede pasar que f(m) y f(b) tengan distinto signo
           (bisect f m b tol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds all the roots that are located between consecutive values of a list
;; of values
;;
;; INPUT:
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; lst: ordered list of real values (lst[i] < lst[i+1])
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;; Whenever sgn(f(lst[i])) != sgn(f(lst[i+1])) this function looks for a
;; root in the corresponding interval.
;;
;; OUTPUT:
;; A list o real values containing the roots of the function in the
;; given sub-intervals
;;
(defun allroot (f lst tol)
  (if (null (rest lst))                                   ; condicion de parada (si queda solo un elemento, para)
      nil
    (let ((root (bisect f (first lst) (second lst) tol)))
      (if (null root)                                     ; si no puedo encontrar raiz en el intervalo
          (allroot f (rest lst) tol)                      ; devuelvo las siguientes
        (cons root                                        ; si la encuentro, la añado a la lista
              (allroot f (rest lst) tol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Divides an interval up to a specified length and find all the roots of
;; the function f in the intervals thus obtained.
;;
;; INPUT:
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; N: Exponent of the number of intervals in which [a,b] is to be divided:
;; [a,b] is divided into 2^N intervals
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;; The interval (a,b) is divided in intervals (x[i], x[i+i]) with
;; x[i]= a + i*dlt; a root is sought in each interval, and all the roots
;; thus found are assembled into a list that is returned.
;;
;; OUTPUT: List with all the found roots.
;;
;; Hint:
;; One might find a way to use allroot to implement this function. This is
;; possible, of course, but there is a simple way of doing it recursively
;; without using allroot.
;;
(defun allind (f a b N tol)
  (if (= N 0)                                ; N va a ir disminuyendo segun divida el intervalo. Cuando sea 0
      (remove nil (list (bisect f a b tol))) ; aplico bisect al intervalo obtenido (lo hago lista para poder concatenarlo luego y le quito posibles nil)
    (let ((m (/ (+ a b) 2)))                 ; punto medio
      (append (allind f a m (- N 1) tol)     ; Si N no es 0, entonces concateno las raices de la primera mitad
              (allind f m b (- N 1) tol))))) ; con las de la segunda mitad

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Combina el elemento dado con cada elemento de la lista dada
;;;
;;; INPUT: elt: elemento (atomo) a combinar con los elementos de la lista
;;; lst: lista de elementos con cada uno de los cuales ha de combinarse el elemento para formar una lista combinada
;;; OUTPUT: Lista de pares (elt, elemento_lista)
;;;
(defun combine-elt-lst (ele lst)
  (mapcar #'(lambda (list1)                  ; para cada elemento list1 de la lista 
              (list ele list1)) lst))        ; creo una lista (ele list1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: una de las listas de las que se debe calcular el producto cartesiano 
;;; lst2: la otra lista de las que se debe calcular el producto cartesiano
;;; OUTPUT: lista producto cartesiano de lst1 y lst2
;;;
(defun combine-lst-lst (lst1 lst2)
  (mapcan #'(lambda (e)                   ; cada elemento de la primera lista
              (combine-elt-lst e lst2))   ; lo combino con toda la segunda lista
    lst1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proper-list (l)
;;; Comprueba si el argumento de enetrada es una lista bien formada
;;;
;;; INPUT: l: argumento a comprobar
;;; OUTPUT: T si l es una lista bien formada, NIL en caso contrario
;;;
(defun proper-list(l)
  (or (null l)
      (and (consp l)
           (proper-list (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst-alt (elt lst)
;;; Combina el elemento dado con cada elemento de la lista dada
;;; Si el elemento es una lista, le anyade el otro al final,
;;; en lugar de formar una nueva lista
;;;
;;; INPUT: elt: elemento (atomo) a combinar con los elementos de la lista
;;; lst: lista de elementos con cada uno de los cuales ha de combinarse el elemento para formar una lista combinada
;;; OUTPUT: Lista de pares (elt, elemento_lista)
;;;
(defun combine-elt-lst-alt (ele lst)
  (mapcar #'(lambda (list1)
              (if (proper-list ele)          ; si el elemento es una lista
                  (append ele (list list1))  ; anyade list1 al final (en lugar de hacer una lista de dos elementos (ele list1))
                (list ele list1)))
    lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst-alt (lst1 lst2)
;;; Calcula el producto cartesiano de dos listas, utilizando combine-elt-lst-alt
;;;
;;; INPUT: lst1: una de las listas de las que se debe calcular el producto cartesiano 
;;; lst2: la otra lista de las que se debe calcular el producto cartesiano
;;; OUTPUT: lista producto cartesiano de lst1 y lst2
;;;
(defun combine-lst-lst-alt (lst1 lst2)
  (mapcan #'(lambda (e)
              (combine-elt-lst-alt e lst2))
    lst1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Calcula todas las posibles disposiciones de elementos pertenecientes
;;; a cada una de las listas en lstolsts
;;;
;;; INPUT: lstolsts: lista de listas 
;;; OUTPUT: lista con todas las posibles combinaciones de los elementos de
;;;  las listas de lstolsts, tomando un solo elemento de cada una de esas listas
;;;
(defun combine-list-of-lsts (lstolsts)
  (if (null (rest lstolsts))                   ; si tiene una sola lista
      (mapcar #'list (first lstolsts))         ; debe satisfacer el ejemplo 4
    (reduce #'combine-lst-lst-alt lstolsts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '¬)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p   x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  (and (atom x)
       (not (connector-p x))
       (not (truth-value-p x))))

;; EJEMPLOS:
(positive-literal-p 'p)
;; evalua a T
(positive-literal-p T)
(positive-literal-p NIL)
(positive-literal-p '¬)
(positive-literal-p '=>)
(positive-literal-p '(p))
(positive-literal-p '(¬ p))
(positive-literal-p '(¬ (v p q)))
;; evaluan a NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
  (and (listp x)
       (not (null x))
       (eql (first x) +not+)
       (positive-literal-p (second x))
       (null (cddr x))))

;; EJEMPLOS:
(negative-literal-p '(¬ p))        ; T
(negative-literal-p NIL)           ; NIL
(negative-literal-p '¬)            ; NIL
(negative-literal-p '=>)           ; NIL
(negative-literal-p '(p))          ; NIL
(negative-literal-p '((¬ p)))      ; NIL
(negative-literal-p '(¬ T))        ; NIL
(negative-literal-p '(¬ NIL))      ; NIL
(negative-literal-p '(¬ =>))       ; NIL
(negative-literal-p 'p)            ; NIL
(negative-literal-p '((¬ p)))      ; NIL
(negative-literal-p '(¬ (v p q)))  ; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  (or (positive-literal-p x)
      (negative-literal-p x))
  )

;; EJEMPLOS:
(literal-p 'p)             
(literal-p '(¬ p))      
;;; evaluan a T
(literal-p '(p))
(literal-p '(¬ (v p q)))
;;; evaluan a  NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un conector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1)))) 
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))               
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))   
                (t NIL)))))))                   ;; No es FBF en formato prefijo 
;;
;; EJEMPLOS:
(wff-prefix-p '(v))
(wff-prefix-p '(^))
(wff-prefix-p '(v A))
(wff-prefix-p '(^ (¬ B)))
(wff-prefix-p '(v A (¬ B)))
(wff-prefix-p '(v (¬ B) A ))
(wff-prefix-p '(^ (v P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;;; evaluan a T
(wff-prefix-p 'NIL)
(wff-prefix-p '(¬))
(wff-prefix-p '(=>))
(wff-prefix-p '(<=>))
(wff-prefix-p '(^ (v P (=> A ( B ^ (¬ C) ^ D))) (^ (<=> P (¬ Q)) P) E))
;;; evaluan a NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-p (x)
  (unless (null x)              ;; NIL no es FBF en formato infijo (por convencion)
    (or (literal-p x)           ;; Un literal es FBF en formato infijo
        (and (listp x)          ;; En caso de que no sea un literal debe ser una lista
             (let* ((op_1      (first x))
                    (conn_1    (second x))
                    (rest_1    (cddr x))
                    (op_2      (first rest_1))
                    (rest_2    (rest rest_1))
                    (conn_2    (first rest_2)))
               (cond
                ((unary-connector-p op_1)       ;; Si el primer elemento es un conector unario
                 (and (null rest_1)             ;; deberia tener la estructura (<conector> FBF)
                      (wff-infix-p conn_1)))
                ((n-ary-connector-p op_1)       ;; Si el primer elemento es un conector enario
                 (null (rest x)))               ;; conjuncion o disyuncion vacias
                ((binary-connector-p conn_1)    ;; Si el segundo elemento es un conector binario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (FBF1 <conector> FBF2)
                      (wff-infix-p op_1)
                      (wff-infix-p op_2)))
                ((n-ary-connector-p conn_1)     ;; Si el segundo elemento es un conector enario
                 (and (wff-infix-p op_1)        ;; el primer operando tiene que ser FBF
                      (or (and (null rest_2)            ;; si solo hay dos operandos
                               (wff-infix-p op_2))      ;; el segundo debe ser una FBF
                          (and (eql conn_2 conn_1)      ;; si hay mas operandos, el segundo conector debe ser igual al primero
                               (wff-infix-p rest_1))))) ;; y quitando el primer operando y conector, debe ser una FBF
                (t NIL)))))))                   ;; No es FBF en formato infijo 

;;
;; EJEMPLOS:
;;
(wff-infix-p 'a)                                ; T
(wff-infix-p '(^))                              ; T  ;; por convencion
(wff-infix-p '(v))                              ; T  ;; por convencion
(wff-infix-p '(A ^ (v)))                        ; T  
(wff-infix-p '( a ^ b ^ (p v q) ^ (¬ r) ^ s))   ; T 
(wff-infix-p '(A => B))                         ; T
(wff-infix-p '(A => (B <=> C)))                 ; T
(wff-infix-p '( B => (A ^ C ^ D)))              ; T   
(wff-infix-p '( B => (A ^ C)))                  ; T 
(wff-infix-p '( B ^ (A ^ C)))                   ; T 
(wff-infix-p '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e))  ; T 
(wff-infix-p nil)                               ; NIL
(wff-infix-p '(a ^))                            ; NIL
(wff-infix-p '(^ a))                            ; NIL
(wff-infix-p '(a))                              ; NIL
(wff-infix-p '((a)))                            ; NIL
(wff-infix-p '((a) b))                          ; NIL
(wff-infix-p '(^ a b q (¬ r) s))                ; NIL 
(wff-infix-p '( B => A C))                      ; NIL   
(wff-infix-p '( => A))                          ; NIL   
(wff-infix-p '(A =>))                           ; NIL   
(wff-infix-p '(A => B <=> C))                   ; NIL
(wff-infix-p '( B => (A ^ C v D)))              ; NIL   
(wff-infix-p '( B ^ C v D ))                    ; NIL 
(wff-infix-p '((p v (a => e (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e)); NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector    (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector) 
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector) 
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector) 
          (cond 
           ((null elements-wff)        ;;; conjuncion o disyuncion vacias. 
            wff)                       ;;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))  
           (t (cons (prefix-to-infix (first elements-wff)) 
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) 
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;;  EJEMPLOS:
;;
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (¬ a)))    ; (¬ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;;; ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d))))) ; (P V (A => (B ^ (¬ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))     ; (((P <=> (¬ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (¬ p) q (¬ r) (¬ s)))       ; ((¬ P) V Q V (¬ R) V (¬ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| (defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let ((op_1      (first wff))
            (connector (second wff))
            (op_2      (third wff))
            (rest_1    (cddr wff)))
        (cond
         ((unary-connector-p op_1)
          (list op_1 (infix-to-prefix connector)))
         ((n-ary-connector-p op_1)           ;; conjuncion o disyuncion vacias.
          wff)
         ((binary-connector-p connector)
          (list connector
                (infix-to-prefix op_1)
                (infix-to-prefix op_2)))
         ((n-ary-connector-p connector)
          (cons connector                     ;; Quitando el primer operando y conector
                (cons (infix-to-prefix op_1)  ;; tambien es FBF infijo
                      (if (null (rest rest_1))                      ;; cuando llego al ultimo operando
                          (list (infix-to-prefix (first rest_1)))   ;; me quedo con el, convertido a prefijo (list y first para que cuadre el caso base de la recursion)
                        (rest (infix-to-prefix rest_1))))))         ;; en los demas, el primer elemento es el conector
         (t NIL)))))) ;; no deberia llegar a este paso nunca
 |#
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let ((op_1      (first wff))
            (connector (second wff))
            (op_2      (third wff))
            (rest_1    (cddr wff)))
        (cond
         ((unary-connector-p op_1)
          (list op_1 (infix-to-prefix connector)))
         ((n-ary-connector-p op_1)           ;; conjuncion o disyuncion vacias.
          wff)
         ((binary-connector-p connector)
          (list connector
                (infix-to-prefix op_1)
                (infix-to-prefix op_2)))
         ((n-ary-connector-p connector)
          (cons connector                                   ;; concateno el conector
                (mapcan #'(lambda (x)
                                  (unless (connector-p x)   ;; con los operandos en infijo
                                    (list (infix-to-prefix x))))
                  wff))))))))

;;
;; EJEMPLOS
;;
(infix-to-prefix nil)      ;; NIL
(infix-to-prefix 'a)       ;; a
(infix-to-prefix '((a)))   ;; NIL
(infix-to-prefix '(a))     ;; NIL
(infix-to-prefix '(((a)))) ;; NIL
(prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)) ) 
;;-> ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)


(infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)

(infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; (¬ (V (¬ P) Q (¬ R) (¬ S)))


(infix-to-prefix
 (prefix-to-infix
  '(V (¬ P) Q (¬ R) (¬ S))))
;;-> (V (¬ P) Q (¬ R) (¬ S))

(infix-to-prefix
 (prefix-to-infix
  '(¬ (V (¬ P) Q (¬ R) (¬ S)))))
;;-> (¬ (V (¬ P) Q (¬ R) (¬ S)))


(infix-to-prefix 'a)  ; A
(infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)

(infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; (¬ (V (¬ P) Q (¬ R) (¬ S)))

(infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d)))))) ; '(v p (=> a (^ b (¬ c) d))))
(infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))) ; '(^ (^ (<=> p (¬ q)) p ) e))  
(infix-to-prefix (prefix-to-infix '( v (¬ p) q (¬ r) (¬ s))))  ; '( v (¬ p) q (¬ r) (¬ s)))
;;;

(infix-to-prefix '(p v (a => (b ^ (¬ c) ^ d)))) ; (V P (=> A (^ B (¬ C) D)))
(infix-to-prefix '(((P <=> (¬ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (¬ Q)) P) E)
(infix-to-prefix '((¬ P) V Q V (¬ R) V (¬ S))); (V (¬ P) Q (¬ R) (¬ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p (wff)
  (when (and (wff-prefix-p wff)
             (not (literal-p wff))
             (eql (first wff) +or+))
    (literals-p (rest wff))))

;; Determina si todos los elementos de una lista son literales
(defun literals-p (lst)
  (or (null lst)      ;; Caso base (recoge clausula vacia)
      (and (literal-p (first lst))     ;; Recursiva y no con mapcan para que pare al encontrar
           (literals-p (rest lst)))))  ;; un elemento no literal, y no recorra toda la lista

;;
;; EJEMPLOS:
;;
(clause-p '(v))             ; T
(clause-p '(v p))           ; T
(clause-p '(v (¬ r)))       ; T
(clause-p '(v p q (¬ r) s)) ; T
(clause-p NIL)                    ; NIL
(clause-p 'p)                     ; NIL
(clause-p '(¬ p))                 ; NIL
(clause-p NIL)                    ; NIL
(clause-p '(p))                   ; NIL
(clause-p '((¬ p)))               ; NIL
(clause-p '(^ a b q (¬ r) s))     ; NIL
(clause-p '(v (^ a b) q (¬ r) s)) ; NIL
(clause-p '(¬ (v p q)))           ; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-p (wff)
  (when (and (wff-prefix-p wff)
             (not (literal-p wff))
             (eql (first wff) +and+))
    (clauses-p (rest wff))))

;; Determina si todos los elementos de una lista son clausulas
(defun clauses-p (lst)
  (or (null lst)      ;; Caso base (recoge conjuncion vacia)
      (and (clause-p (first lst))     ;; Recursiva y no con mapcan para que pare al encontrar
           (clauses-p (rest lst)))))  ;; un elemento no clausula, y no recorra toda la lista

;;
;; EJEMPLOS:
;;
(cnf-p '(^ (v a  b c) (v q r) (v (¬ r) s) (v a b))) ; T
(cnf-p '(^ (v a  b (¬ c)) ))                        ; T
(cnf-p '(^ ))                                       ; T
(cnf-p '(^(v )))                                    ; T
(cnf-p '(¬ p))                                      ; NIL
(cnf-p '(^ a b q (¬ r) s))                          ; NIL
(cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
(cnf-p '(v p q (¬ r) s))                            ; NIL
(cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
(cnf-p '(^ p))                                      ; NIL
(cnf-p '(v ))                                       ; NIL
(cnf-p NIL)                                         ; NIL
(cnf-p '((¬ p)))                                    ; NIL
(cnf-p '(p))                                        ; NIL
(cnf-p '(^ (p)))                                    ; NIL
(cnf-p '((p)))                                      ; NIL
(cnf-p '(^ a b q (r) s))                            ; NIL
(cnf-p '(^ (v a  (v b c)) (v q r) (v (¬ r) s) a b)) ; NIL
(cnf-p '(^ (v a (^ b c)) (^ q r) (v (¬ r) s) a b))  ; NIL
(cnf-p '(¬ (v p q)))                                ; NIL
(cnf-p '(v p q (r) s))                              ; NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-biconditional (wff)
  (if (or (null wff) (literal-p wff))             ;; Un literal no tiene conector bicondicional que eliminar
      wff
    (let ((connector (first wff)))
      (if (eq connector +bicond+)                 ;; si el conector es bicondicional (<=> fbf1 fbf2)
          (let ((wff1 (eliminate-biconditional (second wff)))  ;; elimina bicondicionales de fbf1
                (wff2 (eliminate-biconditional (third  wff)))) ;; elimina bicondicionales de fbf2
            (list +and+                           ;; devuelve (^                              )
                  (list +cond+ wff1 wff2)         ;;             (=> fbf1 fbf2)
                  (list +cond+ wff2 wff1)))       ;;                            (=> fbf2 fbf1)
        (cons connector                                           ;; si es otro conector
              (mapcar #'eliminate-biconditional (rest wff)))))))  ;; elimina bicondicionales de todos los operandos

;;
;; EJEMPLOS:
;;
(eliminate-biconditional '(<=> p  (v q s p) ))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (¬ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (¬ Q)))
;;      (=> (^ S (¬ Q)) (^ (=> P Q) (=> Q P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  
  (if (or (null wff) (literal-p wff))             ;; Un literal no tiene conector condicional que eliminar
      wff
    (let ((connector (first wff)))
      (if (eq connector +cond+)     ;; si el conector es condicional (=> fbf1 fbf2)
          (let ((wff1 (eliminate-conditional (second wff)))  ;; elimina condicionales de fbf1
                (wff2 (eliminate-conditional (third  wff)))) ;; elimina condicionales de fbf2
            (list +or+              ;; devuelve (v              )
                  (list +not+ wff1) ;;             (¬ fbf1)
                  wff2))            ;;                      fbf2
        (cons connector                                         ;; si es otro conector
              (mapcar #'eliminate-conditional (rest wff)))))))  ;; elimina condicionales de todos los operandos       

;;
;; EJEMPLOS:
;;
(eliminate-conditional '(=> p q))                      ;;; (V (¬ P) Q)
(eliminate-conditional '(=> p (v q s p)))              ;;; (V (¬ P) (V Q S P))
(eliminate-conditional '(=> (=> (¬ p) q) (^ s (¬ q)))) ;;; (V (¬ (V (¬ (¬ P)) Q)) (^ S (¬ Q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNF equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce-scope-of-negation (wff)
  (if (or (null wff) (literal-p wff))    ;; Un literal no puede reducir su negacion
      wff
    (let ((connector (first wff)))
      (if (eq connector +not+)                          ;; si el conector es negacion (¬ fbf)
          (let* ((wff_2       (cadr wff))
                 (connector_2 (first wff_2)))
            (cond
             ((eql connector_2 +not+)                   ;; y el conector de la fbf negada tambien es negacion (¬ (¬ fbf2))
              (reduce-scope-of-negation (cadr wff_2)))  ;; elimina las dos negaciones, y devuelve la fbf2 con negaciones reducidas
             ((or (eql connector_2 +and+)           ;; si el conector de la fbf negada es and (¬ (^ fbf2 ... fbfn))
                  (eql connector_2 +or+))           ;; o es or (¬ (v fbf2 ... fbfn))
              (reduce-scope-of-negation             ;; reduzco las negaciones del resultado de
               (cons (exchange-and-or connector_2)  ;; cambiar el conector (and por or y viceversa)
                     (mapcar #'(lambda (x)
                                 (list +not+ x))    ;; y negar
                       (rest wff_2)))))             ;; cada fbf a las que estaba aplicado
             (t NIL))) ;; No deberia darse nunca
        (cons connector                                           ;; si el primer conector no es una negacion
              (mapcar #'reduce-scope-of-negation (rest wff))))))) ;; reduzco el ambito de las negaciones de todas las fbf

(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)    
   ((eq connector +or+) +and+)
   (t connector)))

;;
;;  EJEMPLOS:
;;
(reduce-scope-of-negation '(¬ (v p (¬ q) r))) 
;;; (^ (¬ P) Q (¬ R))
(reduce-scope-of-negation '(¬ (^ p (¬ q) (v  r s (¬ a))))) 
;;;  (V (¬ P) Q (^ (¬ R) (¬ S) A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst- (elt lst)
  (if (null lst)          ;; si la lista es vacia
      (list (list elt))   ;; devuelve ((elt)), que es como si hubiera hecho el caso general con (NIL) como lista
    (mapcar #'(lambda (x) (cons elt x)) lst))) ;; a cada elemento de la lista (que va a ser una fbf) le añade elt delante (... (elt . list[i]) ...)

(defun exchange-NF (nf)               ;; en esencia aplica la propiedad distributiva: (v a (^ b c) d) pasa a ser (^ (v a b d) (v a c d))
  (if (or (null nf) (literal-p nf))   ;; añade los conectores apropiados a lo que devuelve exchange-NF-aux
      nf
    (let ((connector (first nf)))
      (cons (exchange-and-or connector)    ;; cambia el conector (and por or y viceversa)
            (mapcar #'(lambda (x)
                          (cons connector x))     ;; le añade el conector original
                (exchange-NF-aux (rest nf)))))))  ;; a cada una de las combinaciones de los elementos de nf (exchange-nf-aux quita los conectores al combinar)

(defun exchange-NF-aux (nf) ;; aplica la propiedad distributiva, pero sin tener en cuenta los conectores, luego los añade exchange-NF
  (if (null nf)             ;; (a (^ b c) d) pasa a ser ((a b d) (a c d))
      NIL
    (let ((lst (first nf)))
      (mapcan #'(lambda (x)                         ;; cada elemento de nf
                  (combine-elt-lst-                 ;; lo combina
                   x 
                   (exchange-NF-aux (rest nf))))    ;; con todas las combinaciones de los siguientes
        (if (literal-p lst) (list lst) (rest lst))))))  ;; quitando los conectores de haberlos

(defun simplify (connector lst-wffs)  ;; simplifica conectores que son iguales al que se le pasa:
  (if (literal-p lst-wffs)            ;; Con conector v, ((v a b) (^ c d) (v e)) pasa a ser (a b (^ c d) e)
      lst-wffs                    
    (mapcan #'(lambda (x)                     ;; para cada fbf de la lista
                (cond 
                 ((literal-p x) (list x))     ;; si es un literal, lo devuelve (en una lista para poder concatenar)
                 ((equal connector (first x)) ;; si el conector de la fbf es el que busca
                  (mapcan 
                      #'(lambda (y) (simplify connector (list y))) ;; simplifica el mismo conector dentro de la fbf
                    (rest x))) 
                 (t (list x))))               
      lst-wffs)))

(defun cnf (wff)
  (cond
   ((cnf-p wff) wff)      ;; si ya esta en FNC, ya esta
   ((literal-p wff)               ;; si es un literal
    (list +and+ (list +or+ wff))) ;; añade and y or para que este en FNC (^ (v a))
   ((let ((connector (first wff))) 
      (cond
       ((equal +and+ connector)                                   ;; si es una conjuncion
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))  ;; simplifica los and de las fbfs en FNC (^ (^ a b) c) = (^ a b c)
       ((equal +or+ connector)                                           ;; si es una disyuncion
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff))))))))))) ;; simplifica los or, con lo cual queda una disyuncion de literales y conjunciones,
                                                                         ;; les aplica la propiedad distributiva, dejando una conjuncion de disyunciones
                                                                         ;; y finalmente vuelve a llamarse a si misma para simplificar cada una de las fbfs

(cnf 'a)

(cnf '(v (¬ a) b c))
(print (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (¬ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (¬ a) b c) (¬ e) r s 
                (v e f (¬ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (¬ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
(print (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) (^ (¬ p) m (v c d))) (v (¬ a) (¬ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(¬ a))           ; (^ (V (¬ A)))
(cnf '(V (¬ P) (¬ P))) ; (^ (V (¬ P) (¬ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (¬ q)) (v k r (^ m  n))))
;;;   (^ (V P (¬ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (¬ B) D)  (V P Q E F R N (¬ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (¬ B) D)  (V P Q E F M N (¬ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) 
                      (^ (¬ p) m (v c d)))(v (¬ a) (¬ b))) g)))
;;;(^ (V (¬ Y)) (V R S (¬ P)) (V R S M) 
;;;   (V R S C D) (V R (¬ X) (¬ P)) 
;;;   (V R (¬ X) M) (V R (¬ X) C D)
;;;   (V (¬ A) (¬ B)) (V G))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-connectors (cnf)
  (when (cnf-p cnf)
    (eliminate-connectors-rec cnf)))

(defun eliminate-connectors-rec (fbf)
  (mapcar #'(lambda (x)
              (if (literal-p x)
                  x
                (eliminate-connectors-rec x)))
    (rest fbf)))

(eliminate-connectors 'nil)
(eliminate-connectors (cnf '(^ (v p  (¬ q))  (v k  r  (^ m  n)))))
(eliminate-connectors
 (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))

(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
(eliminate-connectors (print (cnf '(^ (v p  (¬ q)) (¬ a) (v k  r  (^ m  n))))))

(eliminate-connectors '(^))
(eliminate-connectors '(^ (v p (¬ q)) (v) (v k r)))
(eliminate-connectors '(^ (v a b)))

;;   EJEMPLOS:
;;

(eliminate-connectors '(^ (v p (¬ q)) (v k r)))
;; ((P (¬ Q)) (K R))
(eliminate-connectors '(^ (v p (¬ q)) (v q (¬ a)) (v s e f) (v b)))
;; ((P (¬ Q)) (Q (¬ A)) (S E F) (B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
  (when (wff-infix-p wff)
    (eliminate-connectors             ;; conectores implicitos
     (cnf                             ;; PASO 4
      (reduce-scope-of-negation       ;; PASO 3
       (eliminate-conditional         ;; PASO 2
        (eliminate-biconditional      ;; PASO 1
         (infix-to-prefix wff)))))))) ;; pasar a infijo

;;
;; EJEMPLOS:
;; 
(wff-infix-to-cnf 'a)
(wff-infix-to-cnf '(¬ a))
(wff-infix-to-cnf  '( (¬ p) v q v (¬ r) v (¬ s)))
(wff-infix-to-cnf  '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e))
;; ((P (¬ A) B) (P (¬ A) (¬ C)) (P (¬ A) D) ((¬ P) (¬ Q)) (Q P) (P) (E))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| (defun eliminate-repeated-literals (k)
  (eliminate-repeated-literals-rec NIL k))

(defun eliminate-repeated-literals-rec (no_rep k)
  (if (null k)
      no_rep
    (let ((1st (first k)))
      (if (member 1st no_rep :test #'equal)
          (eliminate-repeated-literals-rec no_rep (rest k))
        (eliminate-repeated-literals-rec (cons 1st no_rep) (rest k)))))) |#

(defun eliminate-repeated-literals (k)
  (unless (null k)                                  ;; tratando la clausula como un conjunto
    (adjoin (first k)                               ;; añade cada literal
            (eliminate-repeated-literals (rest k))  ;; al resto de la lista sin literales repetidos
            :test #'equal)))

;;
;; EJEMPLO:
;;
(eliminate-repeated-literals '(a b (¬ c) (¬ a) a c (¬ c) c a))
;;;   (B (¬ A) (¬ C) C A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-repeated-clauses (cnf) 
  (unless (null cnf)                                  ;; tratando la FNC como un conjunto
    (adjoin (eliminate-repeated-literals (first cnf)) ;; añade la primera clausula (eliminando sus literales repetidos)
            (eliminate-repeated-clauses (rest cnf))   ;; al resto de la lista sin clausulas repetidas
            :test-not #'(lambda (set1 set2)           ;; comparandolas con igualdad de conjuntos
                          (set-exclusive-or set1 set2 :test #'equal)))))
;; (si xor de dos es nil es porque los dos son vacios o los dos tienen las mismas clausulas)

;; Elimino literales repetidos para cumplir el ejemplo

;;
;; EJEMPLO:
;;
(eliminate-repeated-clauses '(((¬ a) c) (c (¬ a)) ((¬ a) (¬ a) b c b) (a a b) (c (¬ a) b  b) (a b)))
;;; ((C (¬ A)) (C (¬ A) B) (A B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume (K1 K2)
  (when (subsetp K1 K2 :test #'equal) ;; K1 subsume a K2 <=> K1 conenido en K2
    (if (null K1)
        (list K1)
      K1)))
  
;;
;;  EJEMPLOS:
;;
(subsume '(a) '(a b (¬ c)))
;; ((a))
(subsume NIL '(a b (¬ c)))
;; (NIL)
(subsume '(a b (¬ c)) '(a) )
;; NIL
(subsume '( b (¬ c)) '(a b (¬ c)) )
;; ( b (¬ c))
(subsume '(a b (¬ c)) '( b (¬ c)))
;; NIL
(subsume '(a b (¬ c)) '(d  b (¬ c)))
;; nil
(subsume '(a b (¬ c)) '((¬ a) b (¬ c) a))
;; (A B (¬ C))
(subsume '((¬ a) b (¬ c) a) '(a b (¬ c)) )
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-subsumed-clauses (cnf)
  (unless (null cnf)
    (my-adjoin (eliminate-repeated-literals (first cnf))    ;; añade la primera clausula (sin literales repetidos)
               (eliminate-subsumed-clauses (rest cnf)))))   ;; al resto de la NFC ya sin clausulas subsumidas

(defun my-adjoin (k cnf)
  (let ((1st (first cnf))
        (rst (rest cnf)))
    (cond ((null cnf)         ;; Si llega al final de la lista es porque ningun elemento subsume a k
           (list k))          ;; asi que devuelve k
          ((subsume 1st k)    ;; Si un elemento subsume a k
           cnf)               ;; no hace falta añadirlo, se devuelve la cnf tal cual
          ((subsume k 1st)    ;; Si k subsume a un elemento, ese elemento no importa
           (my-adjoin k rst)) ;; asi que se añade k al resto de la cnf
          (t                            ;; Si k no subsume ni es subsumido por el primer elemento
           (cons 1st                    ;; no me salto el elemento, y lo concateno
                 (my-adjoin k rst)))))) ;; con el resultado de añadir k al resto de la cnf

;;
;;  EJEMPLOS:
;;
(eliminate-subsumed-clauses 
 '((a b c) (b c) (a (¬ c) b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((A (¬ C) B) ((¬ A) B) (B C)) ;; el orden no es importante
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (¬ c) b) (b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((B))
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (¬ c) b) ((¬ a))  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((A (¬ C) B) ((¬ A)) (B C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tautology-p (K) 
  (unless (null K)                      ;; NIL es falso
    (let ((1st (first K))
          (rst (rest K)))
      (or (is-negation-of-any 1st rst)  ;; o bien el primer elemento es negacion de alguno de los demas
          (tautology-p rst)))))         ;; o bien se le ha hecho un or con una tautologia

(defun negate-literal (l)
  (if (positive-literal-p l)
      (list +not+ l)
    (cadr l)))

(defun is-negation-of-any (l K)
  (unless (null K)
    (or (equal l (negate-literal (first K)))
        (is-negation-of-any l (rest K)))))

;;
;;  EJEMPLOS:
;;
(tautology-p '((¬ B) A C (¬ A) D)) ;;; T 
(tautology-p '((¬ B) A C D))       ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
  (remove-if #'tautology-p cnf))

;;
;;  EJEMPLOS:
;;
(eliminate-tautologies 
 '(((¬ b) a) (a (¬ a) b c) ( a (¬ b)) (s d (¬ s) (¬ s)) (a)))
;; (((¬ B) A) (A (¬ B)) (A))

(eliminate-tautologies '((a (¬ a) b c)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf (cnf) 
  (eliminate-subsumed-clauses
   (eliminate-tautologies
    (eliminate-repeated-clauses cnf)))) ;; eliminate-repeated-clauses ya elimina los literales repetidos

;;
;;  EJEMPLOS:
;;
(simplify-cnf '((a a) (b) (a) ((¬ b)) ((¬ b)) (a b c a)  (s s d) (b b c a b)))
;; ((B) ((¬ B)) (S D) (A)) ;; en cualquier orden


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ¬lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-neutral-clauses (lambda cnf) 
  (remove-if #'(lambda (k) 
                 (contains-it-or-negation lambda k))
             cnf))

(defun contains-it-or-negation (l k)
  (unless (null k)
    (or (equal (first k) l)
        (equal (first k) (negate-literal l))
        (contains-it-or-negation l (rest k)))))

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((R (¬ S) Q) ((¬ R) S))


(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((P Q) (A B P) (A (¬ P) C))

(extract-neutral-clauses 'p
                           '((p (¬ q) r) (p q) (r (¬ s) p q) (a b p) (a (¬ p) c) ((¬ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-positive-clauses (lambda cnf) 
  (remove-if-not #'(lambda (k)
                     (member lambda k :test #'equal))
                 cnf))

;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))

;; ((P (¬ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((P (¬ Q) R) (R (¬ S) Q))
(extract-positive-clauses 'p
                             '(((¬ p) (¬ q) r) ((¬ p) q) (r (¬ s) (¬ p) q) (a b (¬ p)) ((¬ r) (¬ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ¬lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lambda cnf) 
  (remove-if-not #'(lambda (k)
                     (member (negate-literal lambda) k :test #'equal))
                 cnf))

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((A (¬ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; (((¬ R) S))
(extract-negative-clauses 'p
                             '(( p (¬ q) r) ( p q) (r (¬ s) p q) (a b p) ((¬ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun resolve-on (lambda K1 K2) 
  (let ((¬lambda (negate-literal lambda)))
    (when (or (and (member lambda K1 :test #'equal)
                   (member ¬lambda K2 :test #'equal))
              (and (member lambda K2 :test #'equal)
                   (member ¬lambda K1 :test #'equal)))
      (list 
       (set-difference (union K1 K2 :test #'equal)
                       (list lambda ¬lambda) :test #'equal)))))

;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (¬ c) p) '((¬ p) b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(a b (¬ c) (¬ p)) '( p b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(p) '((¬ p)))
;; (NIL)


(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (¬ c) (¬ p)) '(p b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(a b (¬ c)) '(p b a q r s))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-res (lambda cnf)
  (unless (null cnf)
    (let ((pos-k (extract-positive-clauses lambda cnf))
          (neg-k (extract-negative-clauses lambda cnf))
          (neut-k (extract-neutral-clauses lambda cnf)))
      (eliminate-repeated-clauses
       (append neut-k
               (mapcan #'(lambda (x)
                           (resolve-on lambda (first x) (second x)))
                 (combine-lst-lst-aux pos-k
                                  neg-k)))))))
  
;; Cogidas de un ej anterior
(defun combine-elt-lst-aux (ele lst)
  (mapcar #'(lambda (list1) 
              (list ele list1)) lst))

(defun combine-lst-lst-aux (lst1 lst2)
  (mapcan #'(lambda (e)
              (combine-elt-lst-aux e lst2))
    lst1))

;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (¬ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (¬ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((¬ p))))
;; (NIL)

(build-RES 'q '((p q) ((¬ p) q) (a b q) (p (¬ q)) ((¬ p) (¬ q))))
;; ((P) ((¬ P) P) ((¬ P)) (B A P) (B A (¬ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (¬ q)) (p (¬ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :     T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RES-SAT-p (cnf)
  (if (null cnf)  ;; Si es conjuncion vacia, es tautologia
      T           ;; SAT
    (RES-SAT-p-rec (positive-literals cnf) cnf))) ;; alpha_0 = cnf

(defun RES-SAT-p-rec (pos-lits alpha)
  (cond 
   ((equal alpha (list NIL))  ;; Si llega a la clausula vacia
    NIL)                      ;; UNSAT
   ((null pos-lits) ;; Si no quedan literales positivos, no se puede seguir resolviendo
    T)              ;; SAT
   (T                                                       ;; En cualquier otro caso
    (RES-SAT-p-rec (rest pos-lits)                          ;; con el siguiente literal positivo
                   (simplify-cnf                            ;; simplificar alpha_j
                    (build-RES (first pos-lits) alpha)))))) ;; alpha_j = RES_lambda(alpha_j-1)

(defun positive-literals (cnf) ;; Devuelve los literales positivos de cnf
  (mapcan #'(lambda (k)
              (remove-if #'negative-literal-p k))
    cnf))


;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
(RES-SAT-p nil)  ;;; T
(RES-SAT-p '((p) ((¬ q)))) ;;; T 
(RES-SAT-p
 '((a b d) ((¬ p) q) ((¬ c) a b) ((¬ b) (¬ p) d) (c d (¬ a)))) ;;; T 
(RES-SAT-p
 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) ((¬ q)) ((¬ p) (¬ q) r))) ;;;T
;;
;; UNSAT Examples
;;
(RES-SAT-p '(nil))         ;;; NIL
(RES-SAT-p '((S) nil))     ;;; NIL 
(RES-SAT-p '((p) ((¬ p)))) ;;; NIL
(RES-SAT-p
 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) (p) (q) ((¬ r)) ((¬ p) (¬ q) r))) ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
  (not                            ;; si no es SAT entonces es consecuencia logica
   (RES-SAT-p                     ;; mira si es SAT
    (simplify-cnf                 ;; simplifica cnf
     (wff-infix-to-cnf            ;; convierte en cnf
      (list wff                   ;; (wff      )
            +and+                 ;;      ^
            (list +not+ w)))))))  ;;        ¬w

;;
;;  EJEMPLOS:
;;
(logical-consequence-RES-SAT-p NIL 'a) ;;; NIL
(logical-consequence-RES-SAT-p NIL NIL) ;;; NIL
(logical-consequence-RES-SAT-p '(q ^ (¬ q)) 'a) ;;; T 
(logical-consequence-RES-SAT-p '(q ^ (¬ q)) '(¬ a)) ;;; T 

(logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) '(¬ q))
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'a)
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'a)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'q)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ q))
;; NIL

(or 
 (logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))      ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  'a) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  'q) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  '(¬ q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;En la memoria

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;BFS(grafo G, nodo_fuente s) 
;  { 
;     // recorremos todos los vértices del grafo inicializándolos a NO_VISITADO,
;     // distancia INFINITA y padre de cada nodo NULL
;     for u ∈ V[G] do
;     {
;        estado[u] = NO_VISITADO;
;        distancia[u] = INFINITO; /* distancia infinita si el nodo no es alcanzable */
;        padre[u] = NULL;
;     }
;     estado[s] = VISITADO;
;     distancia[s] = 0;
;     padre[s] = NULL;
;     CrearCola(Q); /* nos aseguramos que la cola está vacía */
;     Encolar(Q, s);
;     while !vacia(Q) do
;     {
;        // extraemos el nodo u de la cola Q y exploramos todos sus nodos adyacentes
;        u = extraer(Q);
;        for  v ∈ adyacencia[u]  do
;        {
;           if estado[v] == NO_VISITADO then
;           {
;                estado[v] = VISITADO;
;                distancia[v] = distancia[u] + 1;
;                padre[v] = u;
;                Encolar(Q, v);
;           }
;        }
;     }
;  }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Apartado siguiente

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new-paths (path node net)
;;; Encuentra nuevos caminos a partir del nodo del grafo dado y me sirve para refrescar mi cola en la funcion bfs
;;;
;;; INPUT: path: lista de caminos actual
;;; node: nodo del cual queremos ver todos los caminos que salen de el
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista de caminos que salen de nuestro nodo 
;;;

(defun new-paths (path node net) 
	(mapcar #'(lambda(n)
				(cons  n  path))
			(rest (assoc node net))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bfs (end queue net)
;;; Breadth-first-search in graphs
;;;
;;; INPUT: end: nodo meta 
;;; queue: lista con los caminos a explorar proximamente y en orden como sublistas
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista de nodos que forman el camino optimo entre el elemento que se le pasa 
;;; en queue y end si bfs tiene exito; NIL si no hay camino
;;;

(defun bfs (end queue net) 
	(if (null queue) '() ;si la cola que nos pasan esta vacia no hay camino
		(let* ((path (first queue)) ;definimos el primer elemento de la cola como path, empezare a explorar por esa rama
			   (node (first path))) ;empiezo a explorar el primer nodo de la rama, node
			(if (eql node end) ;si el nodo que estoy explorando es el nodo meta, ya he acabado
				(reverse path) ;en tal caso, devuelvo el camino que he hecho (lo volteo primero, pues estaba al reves por la logica del programa)
			  (bfs end ;llamo recursivamente a la funcion para que siga explorando en anchura
				(append (rest queue);concateno al final de mi cola el resto de caminos que tengo que explorar(al final para hacer bfs y no dfs)
						(new-paths path node net)) 
				net)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path (start end net)
;;;
;;; Esta funcion realiza la busqueda en anchura (bfs) entre dos nodos de un grafo (net)
;;; enlaces de costes uniformes con lo que si hay solucion, la optima siempre se encontrara a la menor profundidad
;;; con lo que esta funcion encuentra siempre el camino mas corto en el grafo net entre los nodos start y end
;;;
;;; INPUT: start: nodo de partida
;;; end: nodo meta
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista con el camino entre start y end si existe; NIL si no existe tal camino
;;;

(defun shortest-path (start end net)
	(bfs end (list (list start)) net))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
	
;0: (SHORTEST-PATH A F ((A D) (B D F) (C E) (D F) (E B F) (F)))
;    1: (BFS F ((A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;      2: (NEW-PATHS (A) A ((A D) (B D F) (C E) (D F) (E B F) (F)))
;      2: NEW-PATHS returned ((D A))
;      2: (BFS F ((D A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;        3: (NEW-PATHS (D A) D ((A D) (B D F) (C E) (D F) (E B F) (F)))
;       3: NEW-PATHS returned ((F D A))
;       3: (BFS F ((F D A)) ((A D) (B D F) (C E) (D F) (E B F) (F)))
;       3: BFS returned (A D F)
;     2: BFS returned (A D F)
;   1: BFS returned (A D F)
; 0: SHORTEST-PATH returned (A D F)	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf grafo7 '((a b c d e)(b a d e f)(c a g)(d a b g h)(e a b g h)(f b h)(g c d e h)(h d e f g)))

(shortest-path 'f 'c grafo7)
;(F B A C)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5.8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hay-elementos-repetidos (camino-recorrido)
;;;
;;; Funcion para comprobar si hay elementos repetidos en nuestro path
;;;
;;; INPUT: camino-recorrido: nuestro path
;;; OUTPUT: True si hay elementos repetidos en el path (camino-recorrido), NIL (False) si no
;;;

(defun hay-elementos-repetidos (camino-recorrido)
  (or (null camino-recorrido)
      (and (not (member (first camino-recorrido) (rest camino-recorrido)))
           (hay-elementos-repetidos (rest camino-recorrido)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new-paths-improved (path node net)
;;; Version mejorada de la funcion new-paths
;;;
;;; INPUT: path: lista de caminos actual
;;; node: nodo del cual queremos ver todos los caminos que salen de el
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista de caminos que salen de nuestro nodo 
;;;

(defun new-paths-improved (path node net) 
	(if (null (hay-elementos-repetidos path))
		NIL
		(mapcar #'(lambda(n)
					(cons  n  path))
				(rest (assoc node net)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bfs-improved (end queue net)
;;; Breadth-first-search in graphs improved
;;;
;;; INPUT: end: nodo meta 
;;; queue: lista con los caminos a explorar proximamente y en orden como sublistas
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista de nodos que forman el camino optimo entre el elemento que se le pasa 
;;; en queue y end si bfs tiene exito; NIL si no hay camino
;;;

(defun bfs-improved (end queue net) 
	(if (null queue) '() ;si la cola que nos pasan esta vacia no hay camino
		(let* ((path (first queue)) ;definimos el primer elemento de la cola como path, empezare a explorar por esa rama
			   (node (first path))) ;empiezo a explorar el primer nodo de la rama, node
			(if (eql node end) ;si el nodo que estoy explorando es el nodo meta, ya he acabado
				(reverse path) ;en tal caso, devuelvo el camino que he hecho (lo volteo primero, pues estaba al reves por la logica del programa)
			  (bfs-improved end ;llamo recursivamente a la funcion para que siga explorando en anchura
				(append (rest queue);concateno al final de mi cola el resto de caminos que tengo que explorar(al final para hacer bfs y no dfs)
						(new-paths-improved path node net)) 
				net)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved (start end net)
;;;
;;; Version mejorada de la funcion shortest-path
;;;
;;; INPUT: start: nodo de partida
;;; end: nodo meta
;;; net: lista de listas de adyacencia del grafo que queremos explorar
;;; OUTPUT: lista con el camino entre start y end si existe; NIL si no existe tal camino
;;;

(defun shortest-path-improved (start end net)
	(bfs-improved end (list (list start)) net))


(setf grafofalla '((a b c) (b a c) (c a b) (d b)));Grafo con bucle entre a, b y c y un enlace dirigido de d a b 

(shortest-path-improved 'd 'a grafofalla);Es posible llegar de D a cualquier otro nodo, pero no al reves
;(D B A)

(shortest-path-improved 'a 'd grafofalla);No es posible ir de A a D (enlace D->B dirigido y entre A, B, C no dirigidos y bucle)
;NIL