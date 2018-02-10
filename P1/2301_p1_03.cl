;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; is-ok (u v)
;;; Comprueba que dos vectores son del mismo tamaño y no contienen numeros negativos
;;;
;;; INPUT: u: vector, representado como una lista
;;; v: vector, representado como una lista
;;;
;;; OUTPUT: T: cumplen las condiciones
;;; NIL: no cumplen alguna condicion
;;;
(defun is-ok (u v)
           (if (and (null u) (null v))
               t                                                                        ; caso base
               (and (is-ok (rest u) (rest v)) (not                                      ; not para que sea NIL si cumple alguna
                                                   (or (and (null u) (not (null v)))    ; v mas largo que u
                                                       (and (null v) (not (null u)))    ; u mas largo que v
                                                       (minusp (first u))               ; u contiene negativo
                                                       (minusp (first v)))))))          ; v contiene negativo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dot-product-rec (u v)
;;; Calcula el producto escalar de dos vectores de forma recursiva
;;;
;;; INPUT: u: vector, representado como una lista
;;; v: vector, representado como una lista
;;;
;;; OUTPUT: producto escalar de u y v ( uÂ·v )
;;;
(defun dot-product-rec (u v)
	(if (or (null u) (null v))
	   	0
	   	(+ (* (first u)
	   		(first v))
	   	(dot-product-rec (rest u) (rest v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-rec (v)
;;; Calcula la norma-2 (euclÃ­dea) de un vector de forma recursiva
;;;
;;; INPUT: v: vector, representado como una lista
;;;
;;; OUTPUT: norma-2 de v ( ||v|| )
;;;
(defun 2-norm-rec (v)
   (sqrt (dot-product-rec v v))) ; ||v||Â² = <v,v>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-rec (x y)
	(if (not (is-ok x y))
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
;;; OUTPUT: producto escalar de u y v ( uÂ·v )
;;;
(defun dot-product-mapcar (u v)
	(reduce #'+ (mapcar #'* u v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-rec (v)
;;; Calcula la norma-2 (euclÃ­dea) de un vector usando mapcar
;;;
;;; INPUT: v: vector, representado como una lista
;;;
;;; OUTPUT: norma-2 de v ( ||v|| )
;;;
(defun 2-norm-mapcar (v)
	(sqrt (dot-product-mapcar v v))) ; ||v||Â² = <v,v>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-mapcar (x y)
	(if (not (is-ok x y))
		nil
		(/ (dot-product-mapcar x y)
			(* (2-norm-mapcar x)
				(2-norm-mapcar y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;;
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
(defun sc-conf (x vs conf)
	(mapcar #'rest
		(sort
			(remove nil
				(mapcar #'(lambda (z) (sc-conf-ind x z conf)) vs))
			#'> :key #'first)))

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
(defun sc-conf-ind (x v conf)
	(let ((sc (sc-rec x v)))
		(if (or (null sc) (<= sc conf))	; si la similitud se ha podido hacer, y es mayor que conf
			nil
			(cons sc v))))				; mete la similitud como primer elemento, luego la quitaremos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
	(if (null texts)
		nil 													; caso base	
		(cons (reduce #'(lambda (x y)							; saca tupla (id, sc) con el maximo sc
							(if (> (rest x) (rest y))
								x
								y))
						(mapcar #'(lambda (cat)					; saca una lista con tuplas (id, sc) para cada categoria
										(cons (first cat)
										(funcall func (rest cat) (rest (first texts))))) ; aplica la funcion para sacar sc
									cats))
			(sc-classifier cats (rest texts) func))))			; concateno este par con una lista de los pares siguientes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst)
;;; Combina el elemento dado con cada elemento de la lista dada
;;;
;;; INPUT: elt: elemento (atomo) a combinar con los elementos de la lista
;;; lst: lista de elementos con cada uno de los cuales ha de combinarse el elemento para formar una lista combinada
;;; OUTPUT: Lista de pares (elt, elemento_lista)
;;;
(defun combine-elt-lst (ele lst)
	(if(or (null ele) (null lst))
		NIL
		(mapcar #'(lambda (list1) 				; recorro toda la lista y creo u8na lista de listas (ele, ele_lista) 
					(list ele list1)) lst)))        ; list1 el el parametro de mi lambda, lst la lista que nos pasan y la funcion list nos hace listas (ele, ele_lista) 
