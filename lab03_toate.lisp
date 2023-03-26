;1. Sa se construiasca o functie care intoarce adancimea unei liste.

(defun adancime(l)
    (cond
    ((null l) 1)
    ((atom l) 0)
    (T (+ (apply #'max(mapcar #'adancime l)) 1))
    )
)

;2. Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
;care apar, pe orice nivel, dar in aceeasi ordine. De exemplu
;(((A B) C) (D E)) --> (A B C D E)

(defun liniar(l)
    (cond
        ((null l) nil)
        ((atom l) (list l))
        (T (mapcan #'liniar l))
    )
)

;3. Sa se construiasca o functie care verifica daca un atom e membru al
;unei liste nu neaparat liniara.

(defun verifica(l a)
    (cond
        ((atom l) (equal l a))
        (T (some #'identity (mapcar #'(lambda (ls) (verifica ls a)) l)))
    )
)

;4. Sa se construiasca o functie care intoarce suma atomilor numerici
;dintr-o lista, de la orice nivel.

(defun sumaNum(l)
    (cond
        ((numberp l) l)
        ((atom l) 0)
        (T (apply '+(mapcar #'sumaNum l)))
    )
)

;5. Definiti o functie care testeaza apartenenta unui nod intr-un arbore n-ar
;reprezentat sub forma (radacina lista_noduri_subarb1... lista_noduri_subarbn)
;Ex: arborelele este (a (b (c)) (d) (e (f))) si nodul este 'b => adevarat

;la fel ca verifica

;6. Sa se construiasca o functie care intoarce produsul atomilor numerici
;dintr-o lista, de la orice nivel.

(defun prodNum(l)
    (cond
        ((numberp l) l)
        ((atom l) 1)
        (T (apply '*(mapcar #'prodNum l)))
    )
)

;7. Sa se scrie o functie care calculeaza suma numerelor pare minus suma
;numerelor impare la toate nivelurile unei liste.

(defun sumaParImpar(l)
    (cond
        ((and (numberp l) (= (mod l 2) 0)) l)
        ((and (numberp l) (= (mod l 2) 1)) (- 0 l))
        ((atom l) 0)
        (T (apply '+(mapcar #'sumaParImpar l)))
    )
)

;8. Sa se construiasca o functie care intoarce maximul atomilor numerici
;dintr-o lista, de la orice nivel.

(defun maxx(l)
    (cond
        ((numberp l) l)
        ((atom l) nil)
        (T (apply #'max(mapcar #'maxx l)))
    )
)

;9. Definiti o functie care substituie un element E prin elementele
;unei liste L1 la toate nivelurile unei liste date L.

(defun substituie(l e l1)
    (cond
        ((and (atom l) (equal l e)) l1)
        ((atom l) (list l))
        (T (list (apply #'append(mapcar #'(lambda (ls) (substituie ls e l1)) l))))
    )
)

(defun substituie_start(l e l1)
    (car (substituie l e l1))
)

;(substituie_start '(1 a (2 a (4 a 8)) a) 'a '(77 88 99))

;10. Definiti o functie care determina numarul nodurilor de pe nivelul k
;dintr-un arbore n-ar reprezentat sub forma (radacina lista_noduri_subarb1
;... lista_noduri_subarbn) Ex: arborelele este (a (b (c)) (d) (e (f))) si
;k=1 => 3 noduri

(DEFUN noduriNivelK(arb nivel k)
    (COND
    ((and (atom arb) (= nivel k)) 1)
    ((atom arb) 0)
    (t (apply '+ (mapcar #'(lambda (a) (noduriNivelK a (+ 1 nivel) k)) arb)))
    )
)

(defun main_noduriNivelK(arb k)
    (noduriNivelK arb -1 k)
)

;11. Sa se scrie o functie care sterge toate aparitiile unui atom de la
;toate nivelurile unei liste.

(defun sterge(l a)
    (cond
        ((and (atom l) (equal l a)) nil)
        ((atom l) (list l))
        (T (list (apply #'append(mapcar #'(lambda (ls) (sterge ls a)) l))))
    )
)

(defun sterge_start(l a)
    (car (sterge l a))
)

;12. Definiti o functie care inlocuieste un nod cu altul intr-un arbore n-ar
;reprezentat sub forma (radacina lista_noduri_subarb1...lista_noduri_subarbn)
;Ex: arborelele este (a (b (c)) (d) (e (f))) si nodul 'b se inlocuieste cu
;nodul 'g => arborele (a (g (c)) (d) (e (f)))

;12
(defun schimba(n a arb)
  (cond
    ((null arb) nil)
    ((equal arb n) (list a))
    ((atom arb) (list arb))
    (t (list (apply #'append(mapcar #'(lambda (arbore) (schimba n a arbore)) arb))))
  )
)

(defun schimba_main(n a arb)
    (car (schimba n a arb))
)

;la fel ca 13

;15. Sa se construiasca o functie care intoarce numarul atomilor dintr-o
;lista, de la orice nivel.


(DEFUN nrAtomi(lst)
    (COND
    ((atom lst) 1)
    (t (apply '+ (mapcar #'nrAtomi lst)))
    )
)

;16. Definiti o functie care inverseaza o lista impreuna cu toate sublistele
;sale de pe orice nivel.
;Ex: (a b c (d (e)) f) => (f ((e) d) c b a)

(DEFUN invers(L)
    (COND
        ((ATOM L) l)
        (T (MAPCAr #'invers(REVERSE L)))
    )
)
