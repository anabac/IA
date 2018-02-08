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
	   	(+ (* (first u) (first v)) (dot-product-rec (rest u) (rest v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-rec (v)
;;; Calcula la norma-2 (euclídea) de un vector de forma recursiva
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
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-rec (x y)
	(if (or (some #'minusp x) (some #'minusp y))
		nil
		(/ (dot-product-rec x y) (* (2-norm-rec x) (2-norm-rec y))))

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
	(reduce #'+ (mapcar #'* u v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defun 2-norm-rec (v)
;;; Calcula la norma-2 (euclídea) de un vector usando mapcar
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
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-mapcar (x y)
	(if (or (some #'minusp x) (some #'minusp y))
		nil
		(/ (dot-product-mapcar x y) (* (2-norm-mapcar x) (2-norm-mapcar y))))

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
(defun sc-conf (x vs conf) ...)