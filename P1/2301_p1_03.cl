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
				(rest (assoc node net))))

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