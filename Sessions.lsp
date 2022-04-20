
;Devolver el ultimo elemento de la lista
(defun darrer (L)
    (cond 
        ((null (cdr L)) (car L))
        (t (darrer (cdr L)))
    )
)


;Calcular la longitud de una lista
(defun long(l)
    (cond ((null l) 0) 
        (t (+ 1 (long (cdr l))))
    )
)
(long '(a b c d))

;Mirar si un elemento pertenece a la lista
(defun pertany (e l) 
    (cond 
        ((null l) nil)
        ((equal e (car l)) t)
        (t (pertany e (cdr l) )) 
    )
)
(pertany 'a '(a b c d))

;Definir una funcion exponencial
(defun exponent(b e)
    (cond
        ( (and (= b 0) (= e 0)) 'indeterminat)
        ( (= e 0 ) 1)
        (t 
            (* b (exponent b (- e 1) ))
        )
    )
)
(exponent 2 8)


;Fibonacci
(defun fibonacci(x)
    (cond
        ((= x 0) 0)
        ((= x 1) 1)
        (t 
            (+ (fibonacci (- n 1)) fibonacci(- n 2))
        )
    )
)
(fibonacci 6)

;Escriure la funció dividir, que fa la divisió entera de
;M entre N (tots dos positius). Fixau-vos que aquesta
;funció dóna el número de vegades que N està
;contingut dins M
(defun div(m n)
    (cond
        ( (< m n) 0)
        (t 
            (+ 1 (div (- m n) n))
        )
    )
)
(div 9 2)

;Escriure també la funció parell a partir de dividir.
;Aquesta funció ens diu si un número és parell o no
;(igual que evenp)
(defun ispar(x)
    (= (* div(x 2) 2) x) 
)

;Escriure una funció recursiva "senars" que donada
;una llista de números retorna una llista amb tots els
;números parells eliminats
(defun senars (l)
    (cond
        ;Caso base
        ( (null l) nil)
        ;Si es parell pasam d'ell
        ( (evenp (car l)) (senars (cdr l)))
        ;Si es senar
        (t (cons (car l) (senars (cdr l))))
    
    )
)
(senars '(3 1 8 7 4 10))

;Definir la funció borra per eliminar la primera
;aparició d’un element dins una llista
(defun borra (e l)
    (cond
        ;Caso base
        ( (null l) nil)
        ;Si es igual lo borramos
        ( (equal e (car l)) (cdr l))
        ;Si no es igual, guardamos el primer elemento
        (t (cons  (car l) (borra e (cdr l ))))
    )
)
(borra 'a '(1 2 a 3 a b c))

;Borrar todos los elementos iguales de la lista:
(defun borra (e l)
    (cond
        ;Caso base
        ( (null l) nil)
        ;Si es igual lo borramos
        ( (equal e (car l)) (borra e (cdr l)))
        ;Si no es igual, guardamos el primer elemento
        (t (cons (car l) (borra e (cdr l ))))
    )
)

;Definir la funció rdc que torna tots els elements
;d’una llista menys el darrer
(defun rdc (l)
    (cond 
        ;LISTA SOLO TIENE UN ELEMENTO
        ( (null (cdr l)) nil)
        (t (cons (car l) (rdc (cdr l)) ))
    )
)
(rdc '(a b c))

;Definir la funció snoc que afegeix un element al
;final d’una llista
(defun snoc (e l)
    (cond 
        ;Lista vacia, poner elemento
        ((null l) (cons x l))
        ;PARA NO PERDER EL PRIMER ELEMENTO
        (t (cons (car l)(snoc x (cdr l))))
    )
)
(snoc 'a '(b c))

;Definir la funció escala per multiplicar tots els
;elements d’una llista per un número
(defun escala (e l)
    (cond
        ((null l) nil)
        (t (cons (* e (car l)) (escala e (cdr l))) )
    )
)
(escala 2 '(3 5 6))

;Definir les funcions màxim i mínim d’una llista de
;números
(defun maxim (l)
    (cond
        ((null (cdr l)) (car l))
        ( (>= (car l) (maxim (cdr l))) (car l))
        (t (maxim (cdr l)))
    )
)
(maxim '(4 1 3 8 5))

(defun minim (l)
    (cond
        ((null (cdr l)) (car l))
        ( (<= (car l) (minim (cdr l))) (car l))
        (t (minim (cdr l)))
    )
)
(minim '(4 1 3 8 5))

;Escriure una funció per ordenar una llista de
;números amb el mètode de selecció directa (trobar
;el mínim a cada passa i posar-ho al principi)

(defun ordena (l)
    (cond
        ( (null l) nil)
        (t (cons (minim l) (ordena (borra (minim l) l))))
    )
)

; ------------------- FUNCIONES CON FUNCIONES POR  ARGUMENTOS---------------------

;-------------------- I/O --------------------------------------------------------
(read) ; Lee
(print _expresió_ ) ;Escribe la expresion seguido un salto de linea
                    ;OJO, puede provocar escritura del resultado 2 veces

;Printea y multiplica el resutado de la entrada por consola
(defun entrada ()
    (print 'entrada)
    (print (* 3 (read)))
    (entrada)
)

;------------------------ PROPIETATS ---------------------------
;               (putprop àtom valor propietat)
;---------------------------------------------------------------


; Ejer 25/03/2022:

;Escriure la funció invertir que donada una llista la
;gira al revés (sense utilitzar reverse)
(defun invertir (l)
    (cond 
        ((null l) nil)
        (t (append (invertir (cdr l)) (list (cdr l)) ))
    )
)

;Escriure una funció per borrar l’enèsim element
;d’una llista
(defun borrarn (n l)
    (cond
        ((null l) nil)
        ((= n 1) (cdr l))
        (t (cons (car l) (borrarn (- n 1) (cdr l))))
    )
)
(borrarn 3 '(a e i o u))

;Escriure una funció que compti el número de
;vegades que una expressió apareix a una llista
(defun vegades (x l)
    (cond
        ((null l) 0)
        ((equal x (car l)) (+ 1 (vegades x (cdr l))))
        (t (vegades x (cdr l)))
    )
)
(vegades 'a '(1 2 a 3 (a b) a)) → 2
(vegades 'x '(s t (x) 3)) → 0
; ------------------------ Opcion b (contar en listas)
(defun vegadess (x l)
    (cond
        ((null l) 0)
        ((listp (car l)) (+ (vegades x (car l)) (vegades x (cdr l))) )
        ((equal x (car l)) (+ 1 (vegades x (cdr l))))
        (t (vegades x (cdr l)))
    )
)
(vegadess 'a '(1 2 a 3 (a b) a)) → 1
(vegadess 'x '(s t (x) 3)) → 1

;Escriure una funció que compti el número total
;d’àtoms que hi ha dins una llista
(defun atoms (l)
    (cond
        ((null l) 0)
        ;Si el primero es una lista
        ((listp (car l)) (+ (atoms (car l)) (atoms (cdr l))))
        (t (+ 1 (atoms (cdr l))))
    )
)
(atoms '(a (b (c d) e) f g (h i)))