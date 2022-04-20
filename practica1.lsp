;--------------------------------------------------------------------------
;                          PRACTICA LISP - L.P.
;--------------------------------------------------------------------------
;Autors: 
;Juan Carlos Bujosa, Jorge González Pascual, Zhuo Han Yang (杨桌涵)
;--------------------------------------------------------------------------

;Load: (load 'practica1)

;--------------------------
;       COLORS RGB:
;--------------------------
;Negre:     (0, 0, 0)
;Blanc:     (255, 255, 255)
;Vermell:   (210, 0, 0)
;Verd:      (0, 115, 85)
;Blau:      (12, 17, 84)
;Lila:      (72, 39, 155)

;inIt Metodo temporal que crea figuras
(defun init ()
    (cls) ; per netejar la pantalla cada pic que executam una instrucció
    (inicia-patrons)
    (crea-figura 'cub1 'cub '(255 0 0))
    (crea-figura 'cub2 'cub '(255 0 0))
    (crea-figura 'prisma1 'prisma '(255 0 0))
    (crea-figura 'octaedre1 'octaedre '(72 39 155))
    (pinta-figura 'cub1)
    ;(pinta-figura 'prisma1)
    ;(pinta-figura 'octaedre1)
    ;(pinta-figura 'cub2)
)

;Crearà els àtoms pel cub, prisma i octaedre amb totes les seves propietats
(defun inicia-patrons ()
    (inicia-patrons-cub)
    (inicia-patrons-prisma)
    (inicia-patrons-octaedre)
    (putprop 'escena nil 'figures) ;Posar una figura buida per "tenir" la llista inicialitzada
)

;Initzalitza un Prima
(defun inicia-patrons-prisma ()
    ;Punts
    (putprop 'prisma '(
        (-20 -40 0)         ;1
        (20 -40 0)          ;2
        (0 -40 -40)           ;3
        (-20 -40 0)         ;4
        (20 40 0)           ;5
        (0 40 -40))           ;6
    'punts)
    ;Arestes
    (putprop 'prisma '(
        (1 2)   ;1
        (2 3)   ;2
        (3 1)   ;3
        (1 4)   ;4
        (2 5)   ;5
        (3 6)   ;6
        (4 5)   ;7
        (5 6)   ;8
        (6 4))  ;9
    'arestes)
    ;Cares
    (putprop 'prisma '(
        (4 1 5 7)   ;1
        (6 8 5 2)   ;2
        (4 9 6 3)   ;3
        (1 2 3)     ;4
        (7 8 9))    ;5
    'cares)
)

;Initzalitza un Octaedre
(defun inicia-patrons-octaedre ()
    ;Punts
    (putprop 'octaedre '(
        (-20 0 20)    ;1
        (20 0 20)     ;2
        (20 0 -20)    ;3
        (-20 0 -20)   ;4
        (0 -40 0)        ;5
        (0 40 0))        ;6
    'punts)
    ;Arestes
    (putprop 'octaedre '(
        (1 2)   ;1
        (2 3)   ;2
        (3 4)   ;3
        (4 1)   ;4
        (1 6)   ;5
        (2 6)   ;6
        (3 6)   ;7
        (4 6)   ;8
        (1 5)   ;9
        (2 5)   ;10
        (3 5)   ;11
        (4 5))  ;12
    'arestes)
    ;Cares
    (putprop 'octaedre '(
        (1 10 9)    ;1
        (2 10 11)   ;2
        (3 11 12)   ;3
        (4 9 12)    ;4
        (1 5 6)     ;5
        (2 6 7)     ;6
        (3 7 8)     ;7
        (4 8 5))    ;8
    'cares)
)

;Initzalitza un Cub
(defun inicia-patrons-cub ()
    ;Punts
    (putprop 'cub '(
        (-20 -20 20)     ;1
        (20 -20 20)      ;2
        (20 -20 -20)     ;3
        (-20 -20 -20)    ;4
        (-20 20 20)      ;5
        (20 20 20)       ;6
        (20 20 -20)      ;7
        (-20 20 -20))    ;8
    'punts)
    ;Arestes
    (putprop 'cub '(
        (1 2)   ;1
        (2 3)   ;2
        (3 4)   ;3
        (4 1)   ;4
        (1 5)   ;5
        (2 6)   ;6
        (3 7)   ;7
        (4 8)   ;8
        (5 6)   ;9
        (6 7)   ;10
        (7 8)   ;11
        (8 5))  ;12
    'arestes)
    ;Cares
    (putprop 'cub '(
        (1 6 9 5)       ;1
        (2 7 10 6)      ;2
        (3 8 11 7)      ;3
        (4 5 12 8)      ;4
        (1 2 3 4)       ;5
        (9 10 11 12))   ;6
    'cares)
)

;És una funció que permet la creació d'una figura 3D a partir
;del patró triat i guarda aquesta figura dins la propietat 'figures' de tipus llista d'un àtom
;'escena'. 
;--- Paramametres ---
;@n nom
;@p patró
;@c color
(defun crea-figura (n p c)
    (putprop n p 'patro)
    (putprop n c 'color)
    ;Matriu Transformacio
    (putprop n '(
        (1 0 0 0)
        (0 1 0 0)
        (0 0 1 0)
        (0 0 0 1))
    'tmatriu)
    ;Recuperar la llista de figures i posar la nova (per no sobreescriure)
    ;(putprop 'escena (cons n (getlFigures)) 'figures)
    (setFigures (cons n (getlFigures)))
)

;Borra la figura f de l'escena (i de la pantalla)
;--- Paramametres ---
;@f figura a borrar
(defun borra-figura (f)
    ;Pintar de blanco → cls-figura PENDIENTE
    ;Borrar de l'escena
    (setFigures (borra-f f (getlFigures)))
)

;Borra un elemento de una lista
(defun borra-f (f l)
    (cond
        ;Si ya no hay mas figuras
        ( (null l) nil)
        ;Si es igual lo borramos
        ( (equal f (car l)) (cdr l))
        ;Si no es igual, guardamos el primer elemento
        (t (cons (car l) (borra-f f (cdr l))))
    )
)

;Retorna la llista de figures de l'escena
(defun getlFigures ()
    (get 'escena 'figures)
)

;Posa un element/llista a la propietat de figures 
(defun setFigures (e)
    (putprop 'escena e 'figures)
)

;Borra la figura f només de la pantalla
(defun cls-figura (f)
    nil
)
;Borra tot el contingut de l'escena (i de la pantalla)
(defun borra-figures ()
    nil
)

; pinta totes les figures de la llista de figures de l'escena
(defun pinta-figures ()
    (car (get (get (car (get 'escena 'figures)) 'patro) 'arestes))

)

; PATRÓ CUB:
; CARES ((1 6 9 5);1 (2 7 10 6);2 (3 8 11 7);3 (4 5 12 8);4 (1 2 3 4);5 (9 10 11 12);6)
; ARESTES ((1 2);1 (2 3);2 (3 4);3 (4 1);4 (1 5);5 (2 6);6 (3 7);7 (4 8);8 (5 6);9 (6 7);10 (7 8);11 (8 5);12) 
; PUNTS  (-0.5 -0.5 0.5);1 (0.5 -0.5 0.5);2 (0.5 -0.5 -0.5);3 (-0.5 -0.5 -0.5);4 (-0.5 0.5 0.5);5 (0.5 0.5 0.5);6 (0.5 0.5 -0.5);7 (-0.5 0.5 -0.5));8

;Dibuixa una figura de les que han estat creades
;--- Paràmetres ---
;@f figura
(defun pinta-figura (f)
    ;(pinta-cares (get (get (cerca-figura f (get 'escena 'figures)) 'patro) 'cares) (get (cerca-figura f (get 'escena 'figures)) 'patro))
    ;(color (get f 'color))
    (eval (cons 'color (get f 'color))) ; pintam del color que toca la figura
    (pinta-cares (get (get f 'patro) 'cares) (get f 'patro))
    (color 0 0 0) 
)

; Recorr totes les cares pintant totes les arestes
(defun pinta-cares (l p)
    (cond
        ((null l) nil)
        (t  
            (pinta-arestes (car l) p) ; pinta les arestes de la cara
            (pinta-cares (cdr l) p) ; pinta la següent cara
        )
    )
)

; Recorr totes les arestes pintant tots els vèrtexs
(defun pinta-arestes (l p) 
    (cond
        ((null l) nil)
        (t  
            (pinta-vertexs (get-n (car l) (get p 'arestes)) p) ; pinta els vèrtexs de l'aresta
            (pinta-arestes (cdr l) p) ; pinta la següent aresta
        )
    )
)

; Pinta tots els vèrtexs
(defun pinta-vertexs (l p)
    (cond 
        ((null l) nil)
        (t
            ;(print l)
            (move (+ 320 (car (get-n (car l) (get p 'punts))))  (+ 187 (cadr (get-n (car l) (get p 'punts)))) ) ; posicionam el cursor al primer vèrtex
            (draw (+ 320 (car (get-n (cadr l) (get p 'punts)))) (+ 187 (cadr (get-n (cadr l) (get p 'punts)))) ) ; dibuixam l'aresta fins al segon vèrtex
        )
    )
)

; para pruebas: (vertexPerTransformada '(1) '((3 2) (3 4) (5 6) (7 8)))
; Multiplica el vertex per la matriu transformada (necessari per dibuixar les figures)
(defun vertexPerTransformada (v mt)
    ;(add-at-end 1 v) ; necessari per multiplicar el vèrtex (1x4) per la matriu transformada (4x4)
    ;(cons (producteEscalar (add-at-end 1 v) (get-n 1 mt)) (cons (producteEscalar (add-at-end 1 v) (get-n 2 mt)) 
    (cons (producteEscalar (add-at-end 1 v) (get-n 3 mt)) (producteEscalar (add-at-end 1 v) (get-n 4 mt)));))
)

;Calcula el producte escalar de dos vectors donats
;--- Paràmetres ---
;@v1 vector1
;@v2 vector2
(defun producteEscalar (v1 v2)
    (cond 
        ((null v1) 0)
        (t (+ (* (car v1) (car v2)) (producteEscalar (cdr v1) (cdr v2)) ))
    
    )
)

;Cerca una figura dins la llista i la retorna en cas de trobar-la
;--- Paràmetres ---
;@f figura
;@l llista
(defun cerca-figura (f l) ; NO SE UTILIZA AL FINAL (NO BORRAR AUN PORSIACA)
    (cond 
        ((null l)  nil)
        ((EQUAL f (car l)) (car l))
        (t (cerca-figura f (cdr l)))
    )
)

;Afegueix un element al final d'una llista
;--- Paràmetres ---
;@x element
;@l llista
(defun add-at-end (x l)
    (reverse (cons x (reverse l)))
)

;Retorna l'enèssim element d'una llista
;--- Paràmetres ---
;@n enèssim element
;@l llista
(defun get-n (n l) 
	(cond 
		((null l) nil)
		((= n 1) (car l))
		(t (get-n (- n 1) (cdr l)))
	)
)