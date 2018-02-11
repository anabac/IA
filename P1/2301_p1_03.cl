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
(sc-classifier cats texts #'sc-mapcar)






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
  (if (null (rest lst))                                   ; de parada (si queda solo un elemento, para)
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
      (list (bisect f a b tol))              ; aplico bisect al intervalo obtenido (lo hago lista para poder concatenarlo luego)
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
	(if (null lst)				;antes esto: (or (null ele) (null lst)) pero por lo que dijo david por el grupo no dice que ele no pueda ser nil
		NIL
		(mapcar #'(lambda (list1) 				; recorro toda la lista y creo una lista de listas (ele, ele_lista) 
					(list ele list1)) lst)))        ; list1 el el parametro de mi lambda, lst la lista que nos pasan y la funcion list nos hace listas (ele, ele_lista) 


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
	(if (or (null lst1) (null lst2))
		NIL
		(mapcan (lambda (lista1) 								;recorremos la primera lista con un mapcan para concatenar en lugar de poner parentesis
          (mapcar (lambda (lista2) (list lista1 lista2))		;recorremos la segunda lista para cada elemento de la primera y hacemos parejas de dos elementos hasta formar todos los posibles
                  lst2)) lst1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flatten (lst)
;;; Devuelve la lista lst dada sin parentesis anidados (solo los exteriores)
;;;
;;; INPUT: lst: lista con parentesis anidados a aplanar 
;;; OUTPUT: lista aplanada
;;;
(defun flatten (lst) 
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lists-aux (lista_en_construccion lstolsts)
;;; Funcion auxiliar que recibe una lista con los elementos de la primera lista de lstolsts listados para trabajar sobre ella
;;;
;;; INPUT: lista_en_construccion: lista inicializada con los elementos del (first lstolsts) listados, sobre la que se formara la lista final
;;; lstolsts: lista de listas, aunque sera lstolsts sin su first, pues ya tendremos este en forma de lista de cada elemento en lista_en_construccion
;;; OUTPUT: lista con todas las posibles combinaciones de los elementos de las listas de lstolsts, tomando un solo elemento de cada una de esas listas
;;;
(defun combine-list-of-lists-aux (lista_en_construccion lstolsts)
	(if (null lstolsts)																											  ;le pasamos siempre el rest de la anterior (recursion)
		lista_en_construccion																									  ;caso base
		(mapcar #'flatten (combine-list-of-lists-aux (combine-lst-lst lista_en_construccion (first lstolsts)) (rest lstolsts))))) ;la lista sale con parentesis anidados tras usar combine-lst-lst asi que eplanamos cada sublista de la lista

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Calcula todas las posibles disposiciones de elementos pertenecientes a cada una de las listas en lstolsts, a menos que haya mas de 2 listas, en cuyo caso esa tarea la realiza una funcion auxiliar
;;;
;;; INPUT: lstolsts: lista de listas 
;;; OUTPUT: lista con todas las posibles combinaciones de los elementos de las listas de lstolsts, tomando un solo elemento de cada una de esas listas
;;;
(defun combine-list-of-lsts (lstolsts)
	(cond ((some #'null lstolsts) nil)																						;hay alguna lista vacia
		  ((null(rest lstolsts)) (mapcar #'(lambda (x) (list x)) (first lstolsts)))											;hay 1 lista
		  ((null (rest (rest lstolsts))) (combine-lst-lst (first lstolsts) (first (rest lstolsts))))						;hay 2 listas
		  (t (combine-list-of-lists-aux (mapcan #'(lambda (lst1) (list lst1)) (first lstolsts)) (rest lstolsts)))))			;hay 3 o mas listas - realiza el trabajo la funcion auxiliar 		
		  

