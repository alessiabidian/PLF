;PROBLEMA 1 - PROBLEMA 13
;1. Se da un arbore de tipul (1) / (2). Sa se afiseze calea de la radacina pana la un
;nod x dat.
(defun apartine_arbore(l elem)
    (cond 
        ((null l) nil)
        ((listp (car l)) (OR (apartine_arbore (car l) elem) (apartine_arbore (cdr l) elem) ) )
        ((equal (car l) elem) t)
        (t (apartine_arbore (cdr l) elem))
    )
)

(defun path(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) (list (car l)) )
        ((apartine_arbore (cadr l) elem) (cons (car l) (path (cadr l) elem))) ;subarbore stang
        ((apartine_arbore (caddr l) elem) (cons (car l) (path (caddr l) elem) )) ;subarbore drept
        (t NIL)
    )
)

;(path '(A (B) (C (D) (E (F) (G (H) (I))))) 'I)

;---------------------------------------------------------------------------------------------

(defun parcurg_st (l nrNoduri nrMuchii)
  (cond
    ((null l) nil)
    ((= nrNoduri ( + 1 nrMuchii)) nil)
    (t (cons (car l) (cons (cadr l) (parcurg_st (cddr l) (+ 1 nrNoduri) (+ (cadr l) nrMuchii)))))
  )
)

(defun parcurg_dr (l nrNoduri nrMuchii)
  (cond
    ((null l) nil)
    ((= nrNoduri (+ 1 nrMuchii)) l)
    (t (parcurg_dr (cddr l) (+ 1 nrNoduri) (+ (cadr l) nrMuchii)))
  )
)


(defun stang(l)
  (parcurg_st (cddr l) 0 0)
)

(defun drept(l)
  (parcurg_dr (cddr l) 0 0)
)

(defun myAppend(l p)
  (cond
    ((null l) p)
    (t (cons (car l) (myAppend (cdr l) p)))
  )
)

(defun checkExistence(l elem)
  (cond
    ((null l) nil)
    ((equal (car l) elem) t)
    (t (checkExistence (cdr l) elem))
  )
)

(defun checkExistenceLeft(l elem)
  (checkExistence (stang l) elem)
)

(defun checkExistenceRight(l elem)
  (checkExistence (drept l) elem)
)

;(defun path(l elem)
;  (cond
;    ((null l) nil)
;    ((equal (car l) elem) (list elem))
;    ((checkExistenceRight l elem) (cons (car l) (path (drept l) elem)))
;    ((checkExistenceLeft l elem) (cons (car l) (path (stang l) elem)))
;  )
;)
;----------------------------------------------------------------------------------------------------

;(path '(A (B) (C (D) (E (F) (G (H) (I))))) 'I)

;PROBLEMA 2
;2. Sa se tipareasca lista nodurilor de pe nivelul k dintr-un arbore de tipul (1).
(defun nivel_k (l k)
    (cond
        ((null l) NIL)
        ((= k 1) (list (car l)))
        (t (append (nivel_k (cadr l) (- k 1)) (nivel_k (caddr l) (- k 1)) ) )
    )
)

;PROBLEMA 3
;3. Se da un arbore de tipul (1). Sa se precizeze numarul de niveluri din arbore.
;adiCA ADANCIMEA:)))

(defun nr_niveluri(l)
    (cond
        ((null l) 0)
        ((null (cdr l)) 1)
        (t (+ 1 (max (nr_niveluri (cadr l)) (nr_niveluri (caddr l)))))
    )
)

;PROBLEMA 5 - PROBLEMA 10
;5. Sa se intoarca adancimea la care se afla un nod intr-un arbore de tipul (1).

(defun nivel_nod(l elem)
    (cond
        ((null l) NIL)
        ((equal (car l) elem) 0)
        ((apartine_arbore (cadr l) elem) (+ 1 (nivel_nod (cadr l) elem)))
        ((apartine_arbore (caddr l) elem) (+ 1 (nivel_nod (caddr l) elem)))
        (t NIL)
    )
)

;PROBLEMA 6 - PROBLEMA 8
;SRD
(defun inordine(arb)
	(cond
		; frunza
    ((null arb) nil)
		((null (cdr arb))
			(list (car arb))
		)
		(t
			(append
				(inordine (cadr arb))   ;stanga
				(list (car arb))        ;radacina
				(inordine (caddr arb))  ;dreapta
			)
		)
	)
)

;PROBLEMA 11
;11. Se da un arbore de tipul (2). Sa se afiseze nivelul (si lista corespunzatoare
;a nodurilor) avand numar maxim de noduri. Nivelul rad. se considera 0.

; Specificatii: nivelMax(l: arborele dat ca lista, contor)
; => returneaza adancimea arborelui, nr nivelului cel mai adanc/mare
(defun nivelMax(l contor)
  (cond
    ((null l) contor)
    (t (max (nivelMax (cadr l) (+ 1 contor)) (nivelMax (caddr l) (+ 1 contor)))) ;myMax
  )
)

; Specificatii: noduriDePeNivel(l: arborele dat ca lista, nivel: nivelu cautat, contor)
; => returneaza lista cu nodurile de pe nivelul dat ca parametru
(defun noduriDePeNivel(l nivel contor)
  (cond
    ((null l) nil)
    ((equal contor nivel) (list (car l)))
    (t (append (noduriDePeNivel (cadr l) nivel (+ 1 contor)) (noduriDePeNivel (caddr l) nivel (+ 1 contor)))) ;myAppend
  )
)

; doar pt liste liniare
; Specificatii: nrNoduri(l: o lista data liniara, contor: int)
; => nr noduri de pe un nivel dat ca lista liniara
(defun nrNoduri(l contor)
    (cond
      ((null l) contor)
      (t (nrNoduri (cdr l) (+ contor 1)))
    )
)

; Specificatii: gasesteNivMaxNoduri(l: arborele dat ca lista, nivCurent: int)
; => gaseste nr nivelului pe care se afla cele mai multe noduri 
(defun gasesteNivMaxNoduri(l nivCurent)
    (cond
        ((equal nivCurent (nivelMax l -1)) nivCurent)
        ((> (nrNoduri (noduriDePeNivel l nivCurent 0) 0) (nrNoduri (noduriDePeNivel l (gasesteNivMaxNoduri l (+ nivCurent 1)) 0) 0)) nivCurent)
        (t (gasesteNivMaxNoduri l (+ nivCurent 1)))
    )
)

; Specificatii: main(l: arborele dat ca lista)
; => printeaza nr nivelului si nodurile aflate pe nivelul cu cele mai multe noduri
(defun main(l)
  (prin1 (gasesteNivMaxNoduri l 0))
  (noduriDePeNivel l (gasesteNivMaxNoduri l 0) 0)
)

;PROBLEMA 12
;12. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in preordine.
;RSD
(defun preordine(arb)
(cond
		; frunza
        ((null arb) nil)
		((null (cdr arb))
			(list (car arb))
		)
		(t
			(append
                (list (car arb))
				(preordine (cadr arb))
				(preordine (caddr arb))
			)
		)
	)
)

;PROBLEMA 14
;14. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in postordine.
;SDR
(defun postordine(arb)
	(cond
		; frunza
        ((null arb) nil)
		((null (cdr arb))
			(list (car arb))
		)
		(t
			(append
				(postordine (cadr arb))
				(postordine (caddr arb))
                (list (car arb))
			)
		)
	)
)

;PROBLEMA 16
;16. Sa se decida daca un arbore de tipul (2) este echilibrat (diferenta dintre
;adancimile celor 2 subarbori nu este mai mare decat 1).

;(defun nr_niveluri(l)
;    (cond
;        ((null l) 0)
;        ((null (cdr l)) 1)
;        (t (+ 1 (max (nr_niveluri (cadr l)) (nr_niveluri (caddr l)))))
;    )
;)

(defun echilibrat(l)
    (cond
        ((null l) t)
        (t (AND (< (abs (- (nr_niveluri (cadr l)) (nr_niveluri (caddr l)))) 2) (echilibrat (cadr l)) (echilibrat (caddr l))) )
    )
)
