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
		  

