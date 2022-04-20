;PRACTICA LISP - L.P.
;Autors: Juan Carlos Bujosa, Jorge González Pascual, Zhuo Han Yang (杨桌涵)
;Load: (load 'practica1)

;Crearà els àtoms pel cub, prisma i octaedre amb totes les seves propietats
(defun inicia-patrons ()
    (inicia-patrons-cub)
    ;(inicia-patrons-prisma)
    ;(inicia-patrons-octaedre)
    ;(putprop 'escena nil 'figures)
)

;Initzalitza un Prima
(defun inicia-patrons-prisma ()
    ;Punts
    (putprop 'prisma '(
        (-0.5 -1 0)         ;1
        (0.5 -1 0)          ;2
        (0 -1 -1)           ;3
        (-0.5 -1 0)         ;4
        (0.5 1 0)           ;5
        (0 1 -1))           ;6
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
)

;Initzalitza un Octaedre
(defun inicia-patrons-octaedre ()
    nil
)

;Initzalitza un Cub
(defun inicia-patrons-cub ()
    ;Punts
    (putprop 'cub '(
        (-0.5 -0.5 0.5)     ;1
        (0.5 -0.5 0.5)      ;2
        (0.5 -0.5 -0.5)     ;3
        (-0.5 -0.5 -0.5)    ;4
        (-0.5 0.5 0.5)      ;5
        (0.5 0.5 0.5)       ;6
        (0.5 0.5 -0.5)      ;7
        (-0.5 0.5 -0.5))    ;8
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
        (1 2 6 5)   ;1
        (2 3 7 6)   ;2
        (3 4 8 7)   ;3
        (4 1 5 8)   ;4
        (1 2 3 4)   ;5
        (5 6 7 8))  ;6
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
    ;Matriuu Transformacio
    (putprop n '(
        (1 0 0 0)
        (0 1 0 0)
        (0 0 1 0)
        (0 0 0 1))
    'tmatriz)
    ;Recuperar la llista de figures i posar la nova
    (putprop 'escena (cons n (get 'escena 'figures)) 'figures)
)

;Borra la figura f de l'escena (i de la pantalla)
(defun borra-figura (f)
    nil
)
;Borra la figura f només de la pantalla
(defun cls-figura (f)
    nil
)
;Borra tot el contingut de l'escena (i de la pantalla)
(defun borra-figures ()
    nil
)

;Pinta totes les figures de la llista de figures de l'escena
(defun pinta-figures ()
    (car (get (get (car (get 'escena 'figures)) 'patro) 'arestes))

)

;Dibuixa la figura f. A partir de les seves cares, s'han d'agafar les arestes i
;dibuixar-les. Per pintar, basta considerar les coordenades x i y de cada punt, la z no s'ha
;d'utilitzar més que pels càlculs 3D.
(defun pinta-figura (f)
    ;(get (get '(car (get 'escena 'figures)) 'patro) 'punts)
    ;(get 'escena 'figures)
    (get (cerca-figura f (get 'escena 'figures)) 'patro)
    ; de la primera figura, cojo su patron, y con su patron cojo sus puntos
)

;Aquest mètode cercarà una figura concreta de tota l'escena
(defun cerca-figura (f l) 
    (cond 
        ((null l)  nil) ; si no hay ninguna figura, retorna nil
        ((EQUAL f (car l)) (car l))
        (t (cerca-figura f (cdr l)))
    )
)
