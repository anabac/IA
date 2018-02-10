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
                      (if (> (rest x) (rest y))
                          x
                        y))
                  (mapcar #'(lambda (cat)					     ; saca una lista con tuplas (id, sc) para cada categoria
                              (cons (first cat)
                                    (funcall func (rest cat) (rest (first texts))))) ; aplica la funcion para sacar sc
                    cats))
          (sc-classifier cats (rest texts) func))))			             ; concateno este par con una lista de los pares siguientes



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
  (let* ((m (/ (+ a b) 2))
         (fa (funcall f a))
         (fb (funcall f b))
         (fm (funcall f m))
         (siga (> fa 0))
         (sigb (> fb 0))
         (sigm (> fm 0)))
    (cond ((xnor siga sigb)
            nil)
           ((= fa 0)
            a)
           ((= fb 0)
            b)
           ((= fm 0)
            m)
           ((< (- b a) tol)
            m)
           ((xnor sigm sigb)
            (bisect f a m tol))
           (t
            (bisect f m b tol)))))
  
  
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
    (if (or (null ele) (null lst))
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
		(mapcan (lambda (x) 
          (mapcar (lambda (y) (list x y))
                  lst2)) lst1)))
