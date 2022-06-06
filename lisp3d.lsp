;--------------------------------------------------------------------------
;                          PRACTICA LISP3D - L.P.
;--------------------------------------------------------------------------
; Autors: 
; Juan Carlos Bujosa, Jorge González Pascual, Zhuo Han Yang (杨桌涵)
;--------------------------------------------------------------------------

;Load: (load 'lisp3d)

;---------------------------
;       COLORS RGB:
;---------------------------
; Negre:     (0, 0, 0)
; Blanc:     (255, 255, 255)
; Vermell:   (210, 0, 0)
; Verd:      (0, 115, 85)
; Blau:      (12, 17, 84)
; Lila:      (72, 39, 155)
; Turquesa:  (0, 255, 204)
;---------------------------

;Personalització de la consola
(putprop 'letters '(0 255 204) 'color)
(putprop 'background '(0 0 0) 'color)

;Posar els colors de la terminal:
(eval (cons 'color (append (get 'letters 'color) (get 'background 'color)) ) )
(cls)

;inIt Metóde temporal que crea figures
(defun init ()
    (cls) ; per netejar la pantalla cada pic que executam una instrucció
    (inicia-patrons)

    (crea-figura 'cub1 'cub '(255 0 0))
    (escala-figura 'cub1 100 100 100)
    (cls-figura 'cub1)

    (crea-figura 'prisma1 'prisma '(255 0 0))
    (escala-figura 'prisma1 100 100 100)
    (cls-figura 'prisma1)

    (crea-figura 'octaedre1 'octaedre '(72 39 155))
    (escala-figura 'octaedre1 100 100 100)
    (cls-figura 'octaedre1)

    (pinta-figura 'cub1)
)
;------------------------------------------------------------------------------

;Crearà els àtoms pel cub, prisma i octaedre amb totes les seves propietats
(defun inicia-patrons ()
    (inicia-patrons-cub)
    (inicia-patrons-prisma)
    (inicia-patrons-octaedre)
    (putprop 'escena nil 'figures) ;Posar una figura buida per "tenir" la llista inicialitzada
)

;Inicialitza un Prisma
(defun inicia-patrons-prisma ()
    ;Punts
    (putprop 'prisma '(
        (-0.5 -1 0)         ;1
        (0.5 -1 0)          ;2
        (0 -1 -1)           ;3
        (-0.5 1 0)          ;4
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
    ;Cares
    (putprop 'prisma '(
        (4 1 5 7)   ;1
        (6 8 5 2)   ;2
        (4 9 6 3)   ;3
        (1 2 3)     ;4
        (7 8 9))    ;5
    'cares)
)

;Inicialitza un Octaedre
(defun inicia-patrons-octaedre ()
    ;Punts
    (putprop 'octaedre '(
        (-0.5 0 0.5)    ;1
        (0.5 0 0.5)     ;2
        (0.5 0 -0.5)    ;3
        (-0.5 0 -0.5)   ;4
        (0 -1 0)        ;5
        (0 1 0))        ;6
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

;Inicialitza un Cub
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
;--- Paràmetres ---
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

;Mirar si un element pertany a una llista
;--- Paràmetres ---
;@e element
;@l llista
(defun pertany (e l) 
    (cond 
        ((null l) nil)
        ((equal e (car l)) t)
        (t (pertany e (cdr l) )) 
    )
)

;Canvia el color de la figura
;--- Paràmetres ---
;@n nom
;@c color
(defun change-color (n c)
    (putprop n c 'color)
    (pinta-figura n)
)

;Posa la figura f a la seva posició inicial (matriu identitat a la transformació) 
;--- Paràmetres ---
;@f nom de la figura
(defun inicia-figura (f) 
    (putprop f '(
        (1 0 0 0)
        (0 1 0 0)
        (0 0 1 0)
        (0 0 0 1))
    'tmatriu)
)

;Borra la figura f de l'escena (i de la pantalla)
;--- Paràmetres ---
;@f figura a borrar
(defun borra-figura (f)
    ;Repintar
    (cls-figura f)
    ;Borrar figura de l'escena
    (set-figures (borra-f f (get-figures)))
)

;Borra un element d'una llista
;--- Paràmetres ---
;@f figura a borrar
;@l llista
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
;--- Paràmetres ---
;@e figura a posar
(defun set-figures (e)
    (putprop 'escena e 'figures)
)

;Borra la figura f només de la pantalla
;--- Paràmetres ---
;@f figura a borrar
(defun cls-figura (f)
    ;Pintam del color del fons per borrar
    (eval (cons 'color (get 'background 'color) ))
    (pinta-cares (get (get f 'patro) 'cares) (get f 'patro) f)
    ;Tornam a posar el color per defecte
    (eval (cons 'color (get 'letters 'color) ))
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

;Dibuixa una figura de les que han estat creades
;--- Paràmetres ---
;@f figura
(defun pinta-figura (f)
    ;Pintam del color que toca la figura
    (eval (cons 'color (get f 'color))) 
    (pinta-cares (get (get f 'patro) 'cares) (get f 'patro) f)
    ;Possam una altra vegada el color per defecte
    (eval (cons 'color (get 'letters 'color) ))
)

;Recorr totes les cares pintant totes les arestes
;--- Paràmetres ---
;@l cares de la figura
;@p patro de la figura
;@f figura
(defun pinta-cares (l p f)
    (cond
        ((null l) nil)
        (t  
            (pinta-arestes (car l) p f) ; pinta les arestes de la cara
            (pinta-cares (cdr l) p f) ; pinta la següent cara
        )
    )
)

;Recorr totes les arestes pintant tots els vèrtexs
;--- Paràmetres ---
;@l arestes de la figura
;@p patro de la figura
;@f figura
(defun pinta-arestes (l p f)
    (cond
        ((null l) nil)
        (t  
            (pinta-vertexs (get-n (car l) (get p 'arestes)) p f) ; pinta els vèrtexs de l'aresta
            (pinta-arestes (cdr l) p f) ; pinta la següent aresta
        )
    )
)

;Pinta tots els vèrtexs
;--- Paràmetres ---
;@l vertexs de la aresta a pintar
;@p patro de la figura
;@f figura
(defun pinta-vertexs (l p f)
    (cond 
        ((null l) nil)
        (t            
            ;Posicionam el cursor al primer vèrtex
            (move 
                (+ 320 (realpart (round (car (vector-matriu (add-at-end 1 (get-n (car l) (get p 'punts))) (transposta (get f 'tmatriu)))))))   ;Ax
                (+ 187 (realpart (round (cadr (vector-matriu (add-at-end 1 (get-n (car l) (get p 'punts))) (transposta (get f 'tmatriu)))))))  ;Ay
            ) 
            ;Dibuixam l'aresta fins al segon vèrtex
            (draw 
                (+ 320 (realpart (round (car (vector-matriu (add-at-end 1 (get-n (cadr l) (get p 'punts))) (transposta (get f 'tmatriu)))))))   ;Bx
                (+ 187 (realpart (round (cadr (vector-matriu (add-at-end 1 (get-n (cadr l) (get p 'punts))) (transposta (get f 'tmatriu)))))))  ;By
            ) 
        )
    )
)

;Fa la transposta de una matriu
;--- Paràmetres ---
;@l matriu
(defun transposta (l) 
    (cond ((null (car l)) nil) 
        (t (cons (mapcar 'car l) (transposta (mapcar 'cdr l))) ) 
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

;Matriu de translació on es substitueixen els valor de la matriu pels donats a  @param
;La matriu està invertida per a major comoditat en els càlculs
;--- Paràmetres ---
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

;Matriu d'escalat on es substitueixen els valors de la matriu pels donats a @param
;La matriu està invertida per a major comoditat en els càlculs
;--- Paràmetres ---
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

;Matriu d'escalat on es rota @a radians l'eix x 
;La matriu està invertida per a major comoditat en els càlculs
;--- Paràmetres ---
;@a angle en radians
(defun rotax (a)
    (list
    (list 1 0 0 0)
    (list 0 (cos a) (sin a) 0)
    (list 0 (- 0 (sin a)) (cos a) 0)
    (list 0 0 0 1))
)

;Matriu d'escalat on es rota @a radians l'eix y
;La matriu està invertida per a major comoditat en els càlculs
;--- Paràmetres ---
;@a angle en radians
(defun rotay (a)
    (list
        (list (cos a) 0 (sin a) 0)
        (list 0 1 0 0)
        (list (- (sin a)) 0 (cos a) 0)
        (list 0 0 0 1))
)

;Matriu d'escalat on es rota @a radians l'eix z 
;La matriu està invertida per a major comoditat en els càlculs
;--- Paràmetres ---
;@a angle en radians
(defun rotaz (a)
    (list 
    (list (cos a) (sin a) 0 0)
    (list (- (sin a)) (cos a) 0 0)
    (list 0 0 1 0)
    (list 0 0 0 1))
)

;Funció per calcular la nova posició de la figura segons els @param (translació)
;--- Paràmetres ---
;@f figura a canviar
;@x moviment sobre l'eix x
;@y moviment sobre l'eix y
;@z moviment sobre l'eix z
(defun trasllada-figura (f x y z)
    ;per a una millor visualització, borra la figura prèviament i la dibuixa de nou
    (cls-figura f) 

    (putprop f (multiplica-matriu (get f 'tmatriu) (translacio x y z)) 'tmatriu)

    ;es torna a pintar la figura
    (pinta-figura f) 
)

;Funció per calcular la nova posició de la figura segons els @param (rotació)
;--- Paràmetres ---
;@f figura a canviar
;@x rotación sobre l'eix x
;@y rotación sobre l'eix y
;@z rotación sobre l'eix z
(defun rota-figura (f x y z)
    ;borra la figura prèviament
    (cls-figura f) 
    
    (putprop f (multiplica-matriu
        (multiplica-matriu
            (multiplica-matriu (get f 'tmatriu) (rotax x)) 
        (rotay y)) 
    (rotaz z)) 'tmatriu)
    
    ;es torna a pintar la figura
    (pinta-figura f) 
)

;Funció per calcular la nova posició de la figura segons els @param (escalat)
;--- Paràmetres ---
;@f figura a canviar
;@x escalat sobre l'eix x
;@y escalat sobre l'eix y
;@z escalat sobre l'eix z
(defun escala-figura (f x y z)
    ;borra la figura prèviament
    (cls-figura f) 

    (putprop f (multiplica-matriu (get f 'tmatriu) (escalat x y z)) 'tmatriu)

    ;es tornaa pintar la figura
    (pinta-figura f)
)

;Funció que permet multiplicar dues matrius del mateix tamany
;--- Paràmetres ---
;@m1 matriu de transformació de la figura
;@m2 matriu de canvi amb la nova configuració
(defun multiplica-matriu (m1 m2)
    (cond 
        ((null (car m1)) nil)
        (t (cons (vector-matriu (car m1) m2) (multiplica-matriu (cdr m1) m2)))
    )
)

;Funció que permet multiplicar un vector amb una matriu
;--- Paràmetres ---
;@v vector de la figura
;@m matriu de canvi amb la nova configuració
(defun vector-matriu (v m) 
    (cond 
        ((null (car m)) nil)
        (t (cons (sum-list (mapcar '* v (car m))) (vector-matriu v (cdr m)))))
)

;Funció que suma tots els components d'una llista
;--- Paràmetres ---
;@l llista
(defun sum-list (l)
    (cond ((null (car l)) 0)
            (t (+ (car l) (sum-list (cdr l)))))
)

;Mètode que imprimeix al cantó superior dret un text
;--- Paràmetres ---
;@text text
(defun animacio-text (text)
    (goto-xy 70 0)
    (format t text)
    (cleol)
    (values)
)
;Mètode que borra el texte del cantó superior dret
(defun borra-text ()
    (goto-xy 70 0)
    (cleol)
)

;Mode animació (translació, rotació i escalat animats)
;--- Paràmetres ---
;@f figura
(defun animacio (f)
    (animacio-text "ANIMACIO")
    
    (setq key(key-pressed '(114 116 113 101)))
    (cond 
        ((equal key 114) (anima-rotacio f))
        ((equal key 116) (anima-translacio f))
        ((equal key 101) (anima-escalat f))
        ((equal key 113) (animacio-text ""))
    )
)

;Mode rotació animada
;Pitjant les fletxes es mou la figura
;--- Paràmetres ---
;@f figura
(defun anima-rotacio (f)
    (animacio-text "ROTACIO")

        (setq key(key-pressed '(331 333 328 336 113)))
        (cond 
            ((equal key 331) (rota-figura f 0 0.25 0)(anima-rotacio f))             ;izq
            ((equal key 333) (rota-figura f 0 (- 0 0.25) 0)(anima-rotacio f))       ;der
            ((equal key 328) (rota-figura f 0.25 0 0)(anima-rotacio f))             ;arriba
            ((equal key 336) (rota-figura f (- 0 0.25) 0 0)(anima-rotacio f))       ;abajo
            ((equal key 113) (animacio f))
        )
        (cls-figura f)
        (pinta-figura f)
)

;Mode translació animada
;Pitjant les fletxes es mou la figura
;--- Paràmetres ---
;@f figura
(defun anima-translacio (f)
    (animacio-text "TRANSLACIO")

        (setq key(key-pressed '(331 333 328 336 113)))
        (cond 
            ((equal key 331) (trasllada-figura f (- 0 2) 0 0)(anima-translacio f))   ;izq
            ((equal key 333) (trasllada-figura f 2 0 0)(anima-translacio f))         ;der
            ((equal key 328) (trasllada-figura f 0 2 0)(anima-translacio f))         ;arriba
            ((equal key 336) (trasllada-figura f 0 (- 0 2) 0)(anima-translacio f))   ;abajo
            ((equal key 113) (animacio f))
        )
        (cls-figura f)
        (pinta-figura f)
)

;Mode escalat animat
;Pitjant les fletxes es mou la figura
;--- Paràmetres ---
;@f figura
(defun anima-escalat (f)
    (animacio-text "ESCALAT")

        (setq key(key-pressed '(328 336 113)))
        (cond 
            ((equal key 328) (escala-figura f 1.25 1.25 1.25)(anima-escalat f))     ;arriba
            ((equal key 336) (escala-figura f 0.75 0.75 0.75)(anima-escalat f))     ;abajo
            ((equal key 113) (animacio f))
        )
        (cls-figura f)
        (pinta-figura f)
)

;Espera al fet que l'usuari pressioni en una tecla
;i et retorna que tecla ha pressionat si està dins
;de la llista passada per paràmetre
;--- Paràmetres ---
;@l conjunt de posibles tecles que es poden pitjar
(defun key-pressed (l)
    (loop
        (setq key(get-key))
        (when (pertany key l)(return key))
    )
)

;Metode que comprova si un element 
;existeix dins una llista.
;--- Paràmetres ---
;@e element a comprobar
;@l llista de elements
(defun pertany (e l) 
    (cond 
        ((null l) nil)
        ((equal e (car l)) t)
        (t (pertany e (cdr l) )) 
    )
)