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
    (crea-figura 'cub1 'cub '(0 0 0))
    (crea-figura 'prisma1 'prisma '(255 0 0))
    (crea-figura 'octaedre1 'octaedre '(72 39 155))
    ;(pinta-figura 'cub1)
    (pinta-figures)
    ;(pinta-llista-figures '(cub1 prisma1))
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
        (0 -40 -40)         ;3
        (-20 40 0)          ;4
        (20 40 0)           ;5
        (0 40 -40))         ;6
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
        (-20 0 20)      ;1
        (20 0 20)       ;2
        (20 0 -20)      ;3
        (-20 0 -20)     ;4
        (0 -40 0)       ;5
        (0 40 0))       ;6
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
    (cond
        ;Si no existeix es crea de nova
        ((null (pertany n (get-figures))) 
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
                (set-figures (cons n (get-figures)))
        )
    )
)

;Mirar si un elemento pertany a una llista
;--- Paramametres ---
;@e element
;@l llista
(defun pertany (e l) 
    (cond 
        ((null l) nil)
        ((equal e (car l)) t)
        (t (pertany e (cdr l) )) 
    )
)

;Fa un recorregut en la llista de figures i si el troba el canvia el color
;--- Paramametres ---
;@n nom
;@c color
;@l llista
(defun change-color-route (n c l) 
    (cond 
        ((null (get-figures)) nil)
        ((equal n (car (get-figures))) 
            (change-color n c)
        )
        (t (change-color-route n c (cdr (get-figures)) )) 
    )
)

;Canvia el color de la figura
;--- Paramametres ---
;@n nom
;@c color
(defun change-color (n c)
    (putprop n c 'color)
    (pinta-figura n)
)

;Borra la figura f de l'escena (i de la pantalla)
;--- Paramametres ---
;@f figura a borrar
(defun borra-figura (f)
    ;Repintar
    (cls-figura f)
    ;Borrar figura de l'escena
    (set-figures (borra-f f (get-figures)))
)

;Borra un elemento de una lista
(defun borra-f (f l)
    (cond
        ( (null l) nil)
        ( (equal f (car l)) (cdr l))
        (t (cons (car l) (borra-f f (cdr l))))
    )
)

;Retorna la llista de figures de l'escena
(defun get-figures ()
    (get 'escena 'figures)
)

;Posa un element/llista a la propietat de figures 
(defun set-figures (e)
    (putprop 'escena e 'figures)
)

;Borra la figura f només de la pantalla
(defun cls-figura (f)
    (change-color f '(255 255 255))
)
;Borra tot el contingut de l'escena (i de la pantalla)
(defun borra-figures ()
    ;Borrar figures de l'escena
    (set-figures nil)
    (cls) ; per netejar la pantalla cada pic que executam una instrucció
)

;Pinta totes les figures de la llista de figures de l'escena
(defun pinta-figures ()
    (pinta-llista-figures (get-figures))
)

;Pinta totes les digures passades per paràmetre
;--- Paràmetres ---
;@l llista de noms de figures
(defun pinta-llista-figures (l)
    (cond
        ((null l) nil)
        (t  
            (pinta-figura (car l)) ; Pinta la primera figura
            (pinta-llista-figures (cdr l)) ; Iteram
        )
    )
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
    (pinta-cares (get (get f 'patro) 'cares) (get f 'patro) f)
    (color 0 0 0) ; tornam a posar el color negre per defecte
)

; Recorr totes les cares pintant totes les arestes
(defun pinta-cares (l p f)
    (cond
        ((null l) nil)
        (t  
            (pinta-arestes (car l) p f) ; pinta les arestes de la cara
            (pinta-cares (cdr l) p f) ; pinta la següent cara
        )
    )
)

; Recorr totes les arestes pintant tots els vèrtexs
(defun pinta-arestes (l p f)
    (cond
        ((null l) nil)
        (t  
            (pinta-vertexs (get-n (car l) (get p 'arestes)) p f) ; pinta els vèrtexs de l'aresta
            (pinta-arestes (cdr l) p f) ; pinta la següent aresta
        )
    )
)

; Pinta tots els vèrtexs
;--- Paràmetres ---
;@l vertexs de la aresta a pintar
;@p patro de la figura
;@f figura
(defun pinta-vertexs (l p f)
    (cond 
        ((null l) nil)
        (t
            ;(print l)
            ;(move (+ 320 (car (get-n (car l) (vector-per-matriu (get p 'punts) (get f 'tmatriu))   )))  (+ 187 (cadr (get-n (car l) (vector-per-matriu (get p 'punts) (get f 'tmatriu))    )))) ; posicionam el cursor al primer vèrtex
            ;(draw (+ 320 (car (get-n (cadr l) (vector-per-matriu (get p 'punts) (get f 'tmatriu))   ))) (+ 187 (cadr (get-n (cadr l) (vector-per-matriu (get p 'punts) (get f 'tmatriu))   )))) ; dibuixam l'aresta fins al segon vèrtex
            ;(print (get-n (car l) (get p 'punts)))
            ;(print (vector-per-matriu (get-n (car l) (get p 'punts)) (get f 'tmatriu)))
            
            ;Posicionam el cursor al primer vèrtex
            (move 
                (+ 320 (car (vector-per-matriu (get-n (car l) (get p 'punts)) (get f 'tmatriu))))   ;Ax
                (+ 187 (cadr (vector-per-matriu (get-n (car l) (get p 'punts)) (get f 'tmatriu))))  ;Ay
            ) 
            ;Dibuixam l'aresta fins al segon vèrtex
            (draw 
                (+ 320 (car (vector-per-matriu (get-n (cadr l) (get p 'punts)) (get f 'tmatriu))))  ;Bx
                (+ 187 (cadr (vector-per-matriu (get-n (cadr l) (get p 'punts)) (get f 'tmatriu)))) ;By
            ) 
        )
    )
)

; testing: (vector-per-matriu (get 'cub 'punts) (get 'cub1 'tmatriu))  
; para pruebas: (vector-per-matriu '(-20 -20 20) '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))

;Multiplica un vector per una matriu
;--- Paràmetres ---
;@v vector
;@m matriu
(defun vector-per-matriu (v m)
    ; (add-at-end 1 v) Serveix per multiplicar el vèrtex (1x4) per la matriu transformada (4x4)
    (cons (producte-escalar (add-at-end 1 v) (get-n 1 m)) 
        (cons (producte-escalar (add-at-end 1 v) (get-n 2 m)) 
            (cons (producte-escalar (add-at-end 1 v) (get-n 3 m)) 
                (list (producte-escalar (add-at-end 1 v) (get-n 4 m)))
            )
        )
    )
)

;Calcula el producte escalar de dos vectors donats
;--- Paràmetres ---
;@v1 vector1
;@v2 vector2
(defun producte-escalar (v1 v2)
    (cond 
        ((null v1) 0)
        (t (+ (* (car v1) (car v2)) (producte-escalar (cdr v1) (cdr v2)) ))
    )
)

;Cerca una figura dins la llista i la retorna en cas de trobar-la
;--- Paràmetres ---
;@f figura
;@l llista
(defun cerca-figura (f l) ; NO SE UTILIZA AL FINAL (NO BORRAR AUN PORSIACA)
    (cond 
        ((null l)  nil)
        ((equal f (car l)) (car l))
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

;Matriz de translacion donde se sustituyen los valores en la matriz por los dados en @param
;La matriz esta invertida para mayor comodidad en los calculos
;--- Paramametres ---
;@dx eje x
;@dy eje y
;@dz eje z

(defun translacio (dx dy dz)
    (list
    (list 1 0 0 dx)
    (list 0 1 0 dy)
    (list 0 0 1 dz)
    (list 0 0 0 1))
)

;Matriz de escalado donde se sustituyen los valores en la matriz por los dados en @param
;La matriz esta invertida para mayor comodidad en los calculos
;--- Paramametres ---
;@ex eje x
;@ey eje y
;@ez eje z

(defun escalat (ex ey ez)
    (list 
    (list ex 0 0 0)
    (list 0 ey 0 0)
    (list 0 0 ez 0)
    (list 0 0 0 1))
)

;Matriz de escalado donde se rota @a radianes en el eje x 
;La matriz esta invertida para mayor comodidad en los calculos
;--- Paramametres ---
;@a angulo en radianes

(defun rotax (a)
    (list
    (list 1 0 0 0)
    (list 0 (cos a) (sin a) 0)
    (list 0 (- 0 (sin a)) (cos a) 0)
    (list 0 0 0 1))
)

;Matriz de escalado donde se rota @a radianes en el eje y 
;La matriz esta invertida para mayor comodidad en los calculos
;--- Paramametres ---
;@a angulo en radianes

(defun rotay (a)
    (list
        (list (cos a) 0 (sin a) 0)
        (list 0 1 0 0)
        (list (- (sin a)) 0 (cos a) 0)
        (list 0 0 0 1))
)

;Matriz de escalado donde se rota @a radianes en el eje z 
;La matriz esta invertida para mayor comodidad en los calculos
;--- Paramametres ---
;@a angulo en radianes

(defun rotaz (a) 
    (list 
    (list (cos a) (sin a) 0 0)
    (list (- (sin a)) (cos a) 0 0)
    (list 0 0 1 0)
    (list 0 0 0 1))
)

(defun trasllada-figura (f x y z)
   (ACTUALIZAR 'TMATRIU DE F (MULTIPLICAR MATRIZ (get f 'tmatriu) (TRANSLACIO XXX) ))
)

(defun multiplica-matriz ())