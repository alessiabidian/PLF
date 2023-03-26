; 1.
;a) Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea, al 4-lea, al 6-lea,....element.
;b) Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, pe orice nivel, 
;dar in ordine inversa. De exemplu: (((A B) C) (D E)) --> (E D C B A)
;c) Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista neliniara.
;d) Sa se scrie o functie care determina numarul de aparitii ale unui atom dat intr-o lista neliniara.

;a)
(defun insereaza(l poz a)
    (cond
        ((null l) nil)
        ((eq (mod poz 2) 0) (append (list (car l) a) (insereaza (cdr l) (+ poz 1) a)))
        (T (cons (car l) (insereaza (cdr l) (+ poz 1) a)))
    )
)

(defun insereaza_start(l a)
    (insereaza l 1 a)
)

;b)
(defun liniarinvers(l)
    (cond
        ((null l) nil)
        ((atom (car l)) (append (liniarinvers (cdr l)) (list (car l))))
        ((listp (car l)) (append (liniarinvers (cdr l)) (liniarinvers (car l))))
    )
)

;c)
(defun cmmdc(a b)
    (cond
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        ((= 0 b) a)
        ((= 0 a) b)
        ((equal a b) a)
        ((> a b) (cmmdc (- a b) b))
        (T (cmmdc a (- b a)))    
    )
)

(defun cmmdc_lista(l)
    (cond
    ;daca lista ii atom sau daca mai este un singur element in ea gen (7) sau (19)
        ((or (atom l) (and (null (cdr l)) (atom (car l)))) (car l))
        ((atom (car l)) (cmmdc (car l) (cmmdc_lista (cdr l))))
        ((listp (car l)) (cmmdc (cmmdc_lista (car l)) (cmmdc_lista (cdr l))))
    )
)

;d)
(defun aparitii(l a)
    (cond
    ((null l) 0)
    ((equal (car l) a) (+ 1 (aparitii (cdr l) a)))
    ((listp (car l)) (+ (aparitii (car l) a) (aparitii (cdr l) a)))
    (T (aparitii (cdr l) a))
    )
)

;2.
;a) Definiti o functie care selecteaza al n-lea element al unei liste, sau NIL, daca nu exista.
;b) Sa se construiasca o functie care verifica daca un atom e membru al unei liste nu neaparat liniara.
;c) Sa se construiasca lista tuturor sublistelor unei liste. Prin sublista se intelege fie lista insasi, 
;fie un element de pe orice nivel, care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) =>
;=> ( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) ).
;d) Sa se scrie o functie care transforma o lista liniara intr-o multime.

;a)
(defun nelem(l poz n)
    (cond
    ((null l) nil)
    ((equal poz n) (car l))
    (T (nelem (cdr l) (+ poz 1) n))
    )
)

(defun nelem_start(l n)
    (nelem l 1 n)
)

;b)
(defun verifica_apartine(l a)
    (cond
    ((null l) nil)
    ((equal (car l) a) T)
    ((listp (car l)) (or (verifica_apartine (car l) a) (verifica_apartine (cdr l) a)))
    (T (verifica_apartine (cdr l) a))
    )
)

;c)
(defun subliste(l)
    (cond
    ((atom l) nil)
    (T (apply 'append (list l) (mapcar 'subliste l)))
    )
)

;d)
(defun transforma_multime(l)
    (cond
    ((null l) nil)
    ((equal (aparitii (cdr l) (car l)) 0) (cons (car l) (transforma_multime (cdr l))))
    (T (transforma_multime (cdr l)))
    )
)

;3.
;a) Definiti o functie care intoarce produsul a doi vectori.
;b) Sa se construiasca o functie care intoarce adancimea unei liste.
;c) Definiti o functie care sorteaza fara pastrarea dublurilor o lista liniara.
;d) Sa se scrie o functie care intoarce intersectia a doua multimi.

;a)
(defun produs_v(v1 v2)
    (cond
    ((null v1) v2)
    ((null v2) v1)
    (T (cons (* (car v1) (car v2)) (produs_v (cdr v1) (cdr v2))))
    )
)

;b)
(defun adancime(l)
    (cond
    ((null l) 1)
    ((listp (car l)) (max (adancime (cdr l)) (+ (adancime (car l)) 1)))
    (T (adancime (cdr l)))
    )
)

;c)
;insereaza intr o lista sortata un element la locu lui
(defun insert (l e)
  (cond
    ((null l) (list e))
    ((= (car l) e) l)
    ((< e (car l)) (cons e l))
    (t (cons (car l) (insert (cdr l) e)))
  )
)

(defun sortare (l)
  (cond
    ((null l) nil)
    (t (insert (sortare (cdr l)) (car l)))
  )
)

(defun sortare_fara_dubluri(l)
    (sortare (transforma_multime l))
)

;d)

(defun intersectie(m1 m2)
    (cond
    ((null m1) nil)
    ((equal (verifica_apartine m2 (car m1)) T) (cons (car m1) (intersectie (cdr m1) m2)))
    (T (intersectie (cdr m1) m2))
    )
)

;4.
;a) Definiti o functie care intoarce suma a doi vectori.
;b) Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, 
;pe orice nivel, dar in aceeasi ordine. De exemplu:
;(((A B) C) (D E)) --> (A B C D E)
;c) Sa se scrie o functie care plecand de la o lista data ca argument, inverseaza numai 
;secventele continue de atomi. Exemplu:
;(a b c (d (e f) g h i)) ==> (c b a (d (f e) i h g))
;d) Sa se construiasca o functie care intoarce maximul atomilor numerici dintr-o lista, 
;de la nivelul superficial.

;a)
(defun suma_v(v1 v2)
    (cond
    ((null v1) v2)
    ((null v2) v1)
    (T (cons (+ (car v1) (car v2)) (suma_v (cdr v1) (cdr v2))))
    )
)

;b)
(defun liniar(l)
    (cond
        ((null l) nil)
        ((atom (car l)) (cons (car l) (liniar (cdr l))))
        ((listp (car l)) (append (liniar (car l)) (liniar (cdr l))))
    )
)

;c)
(defun inverseaza_continuele (l aux)
  (cond
    ((null l) aux)

    ;daca e lista adaug la final de aux, am terminat cu actuala inversare si lipesc car l inversat si cdr l inversat (appenduite)
    ((listp (car l)) (append aux (append (list (inverseaza_continuele (car l) nil)) (inverseaza_continuele (cdr l) nil))))

    ;adaug practic la inceput de aux -> 1 2 3 va fi 3 2 1 ca adaug la inceput de fiecare data
    (t (inverseaza_continuele (cdr l) (append (list (car l)) aux)))
  )
)

;d)
(defun my_max(a b)
    (cond
    ((equal a nil) b)
    ((equal b nil) a)
    ((< a b) b)
    (T a)
    )
)

(defun max_nr_superficial(l)
    (cond
    ((and (null (cdr l)) (atom (car l))) (car l))
    ((atom (car l)) (my_max (car l) (max_nr_superficial (cdr l))))
    ((listp (car l)) (max_nr_superficial (cdr l)))
    )
)

;5.
;a) Definiti o functie care interclaseaza cu pastrarea dublurilor doua liste liniare sortate.
;b) Definiti o functie care substituie un element E prin elementele unei liste L1 la toate 
;nivelurile unei liste date L.
;c) Definiti o functie care determina suma a doua numere in reprezentare de lista si calculeaza 
;numarul zecimal corespunzator sumei.
;d) Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista liniara.

;a)
(defun interclasare(l1 l2)
    (cond
    ((null l1) l2)
    ((null l2) l1)
    ((< (car l1) (car l2)) (cons (car l1) (interclasare (cdr l1) l2)))
    (T (cons (car l2) (interclasare l1 (cdr l2))))
    )
)

;b)
(defun inlocuire_elem(l elem_vechi lista_noua)
    (cond
        ((null l) nil)
        ((listp (car l)) (cons (inlocuire_elem (car l) elem_vechi lista_noua) (inlocuire_elem (cdr l) elem_vechi lista_noua)))
        ((equal (car l) elem_vechi) (cons lista_noua (inlocuire_elem (cdr l) elem_vechi lista_noua)))
        (t (cons (car l) (inlocuire_elem (cdr l) elem_vechi lista_noua)))
    )
)

;c)

;d)
(defun cmmdc_lista_liniara(l)
    (cond
    ;daca lista ii atom sau daca mai este un singur element in ea gen (7) sau (19)
        ((or (atom l) (and (null (cdr l)) (atom (car l)))) (car l))
        (T (cmmdc (car l) (cmmdc_lista (cdr l))))
    )
)

;6.
;a) Sa se scrie de doua ori elementul de pe pozitia a n-a a unei liste
;liniare. De exemplu, pentru (10 20 30 40 50) si n=3 se va produce (10 20 30 30 40 50).
;b) Sa se scrie o functie care realizeaza o lista de asociere cu cele doua
;liste pe care le primeste. De ex: (A B C) (X Y Z) --> ((A.X) (B.Y) (C.Z)).
;c) Sa se determine numarul tuturor sublistelor unei liste date, pe orice
;nivel. Prin sublista se intelege fie lista insasi, fie un element de pe
;orice nivel, care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) =>
;5 (lista insasi, (3 ...), (4 5), (6 7), (9 10)).
;d) Sa se construiasca o functie care intoarce numarul atomilor dintr-o lista,
;de la nivel superficial.

;a)
(defun dublare_nelem(l poz n)
    (cond
    ((null l) nil)
    ((equal poz n) (append (list (car l)(car l)) (dublare_nelem (cdr l) (+ poz 1) n)))
    (T (cons (car l) (dublare_nelem (cdr l) (+ poz 1) n)))
    )
)

(defun dublare_nelem_start(l n)
    (dublare_nelem l 1 n)
)

;b)
(defun perechi(l1 l2)
    (cond
    ((null l1) nil)
    (T (append (list (append (list (car l1)) (car l2))) (perechi (cdr l1)(cdr l2))))
    )
)

;c)
(defun nr_subliste(l)
    (cond
    ((null l) 1)
    ((listp (car l)) (+ (nr_subliste (car l)) (nr_subliste (cdr l))))
    (T (nr_subliste (cdr l)))
    )
)

;d)
(defun nr_atomi_superficial(l)
    (cond
    ((null l) 0)
    ((atom (car l)) (+ (nr_atomi_superficial (cdr l)) 1))
    (T (nr_atomi_superficial (cdr l)))
    )
)

;7.
;a) Sa se scrie o functie care testeaza daca o lista este liniara.
;b) Definiti o functie care substituie prima aparitie a unui element intr-o lista data.
;c) Sa se inlocuiasca fiecare sublista a unei liste cu ultimul ei element.
;Prin sublista se intelege element de pe primul nivel, care este lista. 
;Exemplu: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)
;(a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)
;d) Definiti o functie care interclaseaza fara pastrarea dublurilor doua liste liniare sortate.

;a)
(defun verif_liniar(l)
    (cond
        ((null l) T)
        ((atom (car l)) (and T (verif_liniar (cdr l))))
        ((listp (car l)) nil)
    )
)

;b)
(defun apare(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) t)
        (t (apare (cdr l) elem))
    )
)

(defun apare_neliniara(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) t)
        ((listp (car l)) (OR (apare_neliniara (car l) elem) (apare_neliniara (cdr l) elem)))
        (t (apare_neliniara (cdr l) elem))
    )
)

(defun prima_aparitie(l e a)
    (cond
    ((null l) nil)
    ((equal (car l) e) (cons a (cdr l)))
    ((and (listp (car l)) (apare_neliniara (car l) e)) (cons (prima_aparitie (car l) e a) (cdr l)))
    (T (cons (car l) (prima_aparitie (cdr l) e a)))
    )
)

;c)
(defun ultim_subliste(l nivel)
    (cond
    ((and (null (cdr l)) (atom (car l))) l)
    ((listp (car l)) (append (ultim_subliste (car l) (+ nivel 1)) (ultim_subliste (cdr l) nivel)))
    ((and (atom (car l)) (= nivel 1)) (cons (car l) (ultim_subliste (cdr l) nivel)))
    (T (ultim_subliste (cdr l) nivel))
    )
)

;d) sau faci interclasare si dupa multime

(defun interclasare2(l1 l2)
    (transforma_multime(interclasare l1 l2))
)

; 8.a)Sa se elimine elementul de pe pozitia a n-a a unei liste liniare.
; Specificatii: eliminapozn(l: lista, poz: pozitia curenta, n: pozitia cautata)
(defun eliminapozn(l poz n)
    (cond
        ((null l) l)
        ((eq poz n) (eliminapozn (cdr l) (+ poz 1) n))
        ((not(eq poz n)) (cons(car l) (eliminapozn (cdr l) (+ poz 1) n)))
    )
)

(defun eliminapozn_start(l n)
    (eliminapozn l '1 n)
)

; b)Definiti o functie care determina succesorul unui numar reprezentat cifra cu cifra intr-o lista.
; De ex: (1 9 3 5 9 9) --> (1 9 3 6 0 0)
; Specificatii: inverseaza(l: lista data neliniara) => returneaza lista inversa
(defun inverseaza(l)
    (cond
        ((null l) nil)
        ((listp (car l)) (append (inverseaza (cdr l)) (list (inverseaza (car l)))))
        (T (append (inverseaza (cdr l)) (list (car l))))
    )
)

; Specificatii: succesorul(l: o lista data liniara, carry: surplus) => returneaza succesorul inversat
(defun succesorul(l carry)
    (cond
        ((and (null l) (not(eq carry 0))) (list carry))
        ((null l) nil)
        ((cons (mod (+ (car l) carry) 10) (succesorul (cdr l) (/ (- (+ (car l) carry) (mod (+ (car l) carry) 10)) 10))  ))
    )
)

; succesor_start(l: lista data) - functie wrapper
(defun succesor_start(l)
    (inverseaza(succesorul (inverseaza l) '1))
)

; c)Sa se construiasca multimea atomilor unei liste.Exemplu: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)
; Specificatii: liniar(l: o lista data neliniara) => returneaza lista liniara
(defun liniar(l)
    (cond
        ((null l) nil)
        ((numberp (car l)) (cons (car l) (liniar (cdr l))))
        ((listp (car l)) (append (liniar (car l)) (liniar (cdr l))))
        (T (liniar (cdr l)))
    )
)

; Specificatii: apare(l: o lista data, e: elementul cautat)
; => returneaza true daca gaseste elementul e in lista l
(defun apare(l e)
    (cond
        ((null l) nil)
        ((and (numberp (car l)) (equal (car l) e)) T)
        ((listp (car l)) (or (apare (car l) e) (apare (cdr l) e)))
        (T (apare (cdr l) e))
    )
)

; doar pt liste liniare
; Specificatii: fa_multime(l: o lista data)
; => multime din lista liniara, sterge repetitiile 
(defun fa_multime(l)
    (cond
        ((null l) l) ;sau nil
        ((apare (cdr l) (car l)) (fa_multime (cdr l)))
        (T (cons (car l) (fa_multime (cdr l))))
    )
)

(defun fa_multime_atomi(l)
    (fa_multime (liniar l))
)

; d)Sa se scrie o functie care testeaza daca o lista liniara este o multime.
; Specificatii: este_multime(l: lista liniara)
; => true daca e multime, false altfel
(defun este_multime(l)
    (cond
        ((null l) T)
        ((apare (cdr l) (car l)) nil)
        (T (este_multime (cdr l)))
    )
)

;9.
;a) Sa se scrie o functie care intoarce diferenta a doua multimi.
;b) Definiti o functie care inverseaza o lista impreuna cu toate sublistele sale de pe orice nivel.
;c) Dandu-se o lista, sa se construiasca lista primelor elemente ale tuturor elementelor lista ce 
;au un numar impar de elemente la nivel superficial. 
;Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10 11)) => (1 3 9).
;d) Sa se construiasca o functie care intoarce suma atomilor numerici dintr-o lista, de la nivelul superficial.

;a)
(defun fa_diferenta_multimi(m1 m2)
    (cond
        ((null m1) nil)
        ((apare m2 (car m1)) (fa_diferenta_multimi (cdr m1) m2))
        (T (cons (car m1) (fa_diferenta_multimi (cdr m1) m2)))
    )
)

;b) Ex: (a b c (d (e)) f) => (f ((e) d) c b a)
(defun inverseaza(l)
    (cond
        ((null l) nil)
        ((atom (car l)) (append (inverseaza (cdr l)) (list (car l))))
        (T (append (inverseaza (cdr l)) (list (inverseaza (car l)))))
    )
)

;c) folosim nr_atomi_superficial
(defun primele_elemente(l)
    (cond
        ((null l) nil)
        ((and (listp (car l)) (= (mod (nr_atomi_superficial (car l)) 2) 1)) (append (list (caar l)) (primele_elemente (car l)) (primele_elemente (cdr l))))
        (T (primele_elemente (cdr l)))
    )
)

(defun main_primele_elemente(l)
    (primele_elemente (list l)) ;ca sa imi ia in considerare si primul atom din lista mare
)

;d)
(defun suma_superficial(l)
    (cond
        ((null l) 0)
        ((numberp (car l)) (+ (car l) (suma_superficial (cdr l))))
        (T (suma_superficial (cdr l)))
    )
)

;10.
;a) Sa se construiasca o functie care intoarce produsul atomilor numerici dintr-o lista, de la nivelul superficial.
;b) Sa se scrie o functie care, primind o lista, intoarce multimea tuturor perechilor din lista. De exemplu: (a b c d) --> ((a b) (a c) (a d)(b c) (b d) (c d))
;c) Sa se determine rezultatul unei expresii aritmetice memorate in preordine pe o stiva. Exemple:
;(+ 1 3) ==> 4 (1 + 3)
;(+ * 2 4 3) ==> 11 ((2 * 4) + 3)
;(+ * 2 4 - 5 * 2 2) ==> 9 ((2 * 4) + (5 - (2 * 2))
;d) Definiti o functie care, dintr-o lista de atomi, produce o lista de perechi (atom n), unde atom apare in lista initiala de n ori. De ex:
;(A B A B A C A) --> ((A 4) (B 2) (C 1)).

;a)
(defun produs_superficial(l)
    (cond
        ((null l) 1)
        ((numberp (car l)) (* (car l) (produs_superficial (cdr l))))
        (T (produs_superficial (cdr l)))
    )
)

;b)
(defun fa_perechi(e l)
    (cond
    ((null l) nil)
    ((equal e (car l)) (fa_perechi e (cdr l)))
    (t (cons (list e (car l)) (fa_perechi e (cdr l)) ))
    )
)

(defun perechi_lista(l)
    (cond
    ((null l) nil)
    (t (append (fa_perechi (car l) (cdr l)) (perechi_lista (cdr l))))
    )
)

;c mereu ma uit dupa combinatia semn op1 op2 si inlocuiesc cu rezultatul pana ajung la un sg numar
(defun fa_operatie(semn nr1 nr2)
    (cond
        ((equal semn '+) (+ nr1 nr2))
        ((equal semn '*) (* nr1 nr2))
        ((equal semn '-) (- nr1 nr2))
    )
)

(defun operatie_mica(l)
    (cond
        ((equal (cdr l) nil) (car l))
        ((AND (not (numberp (car l))) (numberp (cadr l)) (numberp (caddr l)) ) (  append  (list (fa_operatie (car l) (cadr l) (caddr l)))  (cdddr l)  ))
        (t (append (list (car l)) (operatie_mica (cdr l)) ))
    )
)

(defun expresie_preordine(l)
    (cond
        ((null (cdr l)) (car l))
        (t (expresie_preordine (operatie_mica l) ))
    )
)

;d)
(defun nr_aparitii(e l)
    (cond
        ((null l) 0)
        ((equal e (car l)) (+ 1 (nr_aparitii e (cdr l))))
        (t (nr_aparitii e (cdr l)))
    )
)

(defun eliminaElem(e l)
    (cond
        ((null l) nil)
        ((equal e (car l)) (eliminaElem e (cdr l)))
        (t (cons (car l) (eliminaElem e (cdr l))))
    )
)

(defun perechi_frecv(l)
    (cond
    ((null l) nil)
    (t (cons (list (car l) (nr_aparitii (car l) l)) (perechi_frecv (eliminaElem (car l) (cdr l)))))
    )
)

;11.
;a) Sa se determine cel mai mic multiplu comun al valorilor numerice dintr-o lista neliniara.
;b) Sa se scrie o functie care sa testeze daca o lista liniara formata din numere intregi are aspect de "munte"
;(o secvență se spune ca are aspect de "munte" daca elementele cresc pana la un moment dat, apoi descresc. De ex. 10 18 29 17 11 10).
;c) Sa se elimine toate aparitiile elementului numeric maxim dintr-o lista neliniara.
;d) Sa se construiasca o functie care intoarce produsul atomilor numerici pari dintr-o lista, de la orice nivel.

;a)
(defun cmmmc(a b)
    (cond 
        ((not (numberp a)) b)
        ((not (numberp b)) a)
        (t (/ (* a b) (cmmdc a b)) )
    )
)

(defun cmmmc_lista(l)
  (cond
    ((null l) nil)
    ((listp (car l)) (cmmmc (cmmmc_lista (car l)) (cmmmc_lista (cdr l))))
    ((not (numberp (car l))) (cmmdc_lista (cdr l)))
    (t (cmmmc (car l) (cmmmc_lista(cdr l))))
  )
)

;b)
;0 inseamna ca suntem in scadere 1 in crestere
(defun lungime(l)
  (cond
    ((null l) 0)
    (t (+ 1 (lungime (cdr l))))
  )
)

(defun munte(l ok)
    (cond
     ((equal (lungime l) 1) t)
     ((AND (< (car l) (car (cdr l))) (equal ok 1)) (munte (cdr l) 1))
     ((> (car l) (car (cdr l))) (munte (cdr l) 0))
     (t nil)
    )
)

;vedem sa nu scadem din prima
(defun main2(l)
    (cond
        ((< (car l) (car (cdr l))) (munte l 1))
        (t nil)
    )
)

;c) folosim my_max si eliminaEl
;merge pe liniar
(defun gasesteMaxLista(l)
    (cond
    ((null (cdr l)) (car l))
    ((numberp (car l)) (my_max (car l) (gasesteMaxLista (cdr l))))
    ((atom (car l)) (gasesteMaxLista (cdr l)))
    (t (my_max (gasesteMaxLista (car l)) (gasesteMaxLista (cdr l))))
    )
)

(defun stergeElem(e l)
    (cond
        ((null l) nil)
        ((equal e (car l)) (stergeElem e (cdr l)))
        ((atom (car l)) (cons (car l) (stergeElem e (cdr l))))
        (t (cons (stergeElem e (car l)) (stergeElem e (cdr l))))
    )
)

(defun stergeElMax(l)
    (stergeElem (gasesteMaxLista (liniar l)) l)
)

;d)
(defun produs_par(l)
    (cond
    ((null l) 1)
    ((and (numberp (car l)) (= (mod (car l) 2) 0)) (* (car l) (produs_par (cdr l))))
    ((atom (car l)) (produs_par (cdr l)))
    (t (* (produs_par (car l)) (produs_par (cdr l))))
    
    )
)

;12.
;a) Definiti o functie care intoarce produsul scalar a doi vectori.
;b) Sa se construiasca o functie care intoarce maximul atomilor numerici dintr-o lista, de la orice nivel.
;c) Sa se scrie o functie care intoarce lista permutarilor unei liste date.
;d) Sa se scrie o functie care intoarce T daca o lista are numar par de elemente pe primul nivel si NIL in caz contrar,
; fara sa se numere elementele listei.

;a)
(defun produs_vectori(l1 l2)
  (cond
    ((null l1) 0)
    (t (+ (* (car l1) (car l2)) (produs_vectori (cdr l1) (cdr l2))))
  )
)

;b)
(defun lungime(l)
  (cond
    ((null l) 0)
    (t (+ 1 (lungime (cdr l))))
  )
)

(defun max_lista(l)
    (cond
        ((null l) nil)
        ((AND (equal (lungime l) 1) (atom (car l))) (car l))
        ((listp (car l)) (my_max (max_lista (car l)) (max_lista (cdr l))))
        (t (my_max (car l) (max_lista (cdr l))))
    )
)

;c)
;PERMUTARI
(defun permutari(L)
    (cond
        ((null(cdr L))(list L))
        (t (mapcan #'(lambda (e)(mapcar #'(lambda (p) (cons e p)) (permutari(remove e L)))) L))
    )
)
;---------------------------------
;ins poz n a unei liste
(defun ins(e n l)
    (cond
        ((= n 1) (cons e l))
        (t (cons (car l) (ins e (- n 1) (cdr l))))
    )
)

;ins pe toate poz de la n+1 la 1 si combina liste
(defun insert (e n l)
    (cond
        ((= n 0) nil)
        (t (cons (ins e n l) (insert e (- n 1) l)))  
    )
)

;wrapper
(defun inserare(e l)
    (insert e (+ (length l) 1) l)
)

;; l- lista de liste!!!
(defun inserareFiecareLista(e l)
    (cond
        ((null l) nil)
        ;;lista de lista + lista de lista
        (t (append (inserare e (car l)) (inserareFiecareLista e (cdr l))))
    )
)

(defun permutari(l)
    (cond 
        ((null l) (list nil))
        (t (inserareFiecareLista (car l) (permutari (cdr l))))
    )
)

;d)
(defun nr_par(l)
    (cond
        ((null l) t)

        ;aici verific sa existe si al doilea element si sa nu fie nil
        ((not (null (cadr l)) ) (nr_par (cddr l)))
        (t nil)
    )
)

;13.
;a) Sa se intercaleze un element pe pozitia a n-a a unei liste liniare.
;b) Sa se construiasca o functie care intoarce suma atomilor numerici dintr-o lista, de la orice nivel.
;c) Sa se scrie o functie care intoarce multimea tuturor sublistelor unei liste date. 
;Ex: Ptr. lista ((1 2 3) ((4 5) 6)) => ((1 2 3) (4 5) ((4 5) 6))
;d) Sa se scrie o functie care testeaza egalitatea a doua multimi, fara sa se faca apel la diferenta a doua multimi.

;a
(defun intercalare_n(l n elem poz)
    (cond
        ((null l) nil)
        ((equal poz n) (cons elem l))
        (t (cons (car l) (intercalare_n (cdr l) n elem (+ poz 1))))
    )
)

(defun main1(l n elem)
    (intercalare_n l n elem 1)
)

;b
(defun suma(l)
    (cond
        ((null l) 0)
        ((listp (car l)) (+ (suma (cdr l)) (suma(car l)) ))
        ((numberp (car l)) (+ (car l) (suma (cdr l)))) 
        (t (+ 0 (suma (cdr l)))) 
    )
) 

;c
(defun subliste(l)
  (cond
    ((atom l) nil)
    (T (apply 'append (list l) (mapcar 'subliste l)))
  )
)

;d
(defun sterge_prima_aparitie(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) (cdr l))
        (t (append (list (car l)) (sterge_prima_aparitie (cdr l) elem)))
    )
)

(defun apare(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) t)
        (t (apare (cdr l) elem))
    )
)

(defun egalitate(l1 l2)
    (cond
        ((AND (null l1) (null l2)) t)
        ((apare l2 (car l1)) (egalitate (cdr l1) (sterge_prima_aparitie l2 (car l1))))
        (t nil)
    )
)

;14.
;a) Dandu-se o lista liniare, se cere sa se elimine elementele din N in N.
;b) Sa se scrie o functie care sa testeze daca o lista liniara formata din numere intregi are aspect de "vale"
;(o secvență se spune ca are aspect de "vale" daca elementele descresc pana la un moment dat, apoi cresc. De ex. 10 8 6 17 19 20).
;c) Sa se construiasca o functie care intoarce minimul atomilor numerici dintr-o lista, de la orice nivel.
;d) Sa se scrie o functie care sterge dintr-o lista liniara toate aparitiile elementului maxim numeric.

;a
(defun elinima_din_n_in_n(l n ok)
    (cond
        ((null l) nil)
        ((equal ok 1) (elinima_din_n_in_n (cdr l) n n))
        (t (cons (car l) (elinima_din_n_in_n (cdr l) n (- ok 1))))
    )
)

(defun main1(l n)
    (elinima_din_n_in_n l n n)
)

;b
;0 inseamna ca suntem in scadere 1 in crestere
;(defun lungime(l)
;  (cond
;    ((null l) 0)
;    (t (+ 1 (lungime (cdr l))))
;  )
;)

(defun vale(l ok)
    (cond
     ((equal (lungime l) 1) t)
     ((AND (> (car l) (car (cdr l))) (equal ok 0)) (vale (cdr l) 0))
     ((< (car l) (car (cdr l))) (vale (cdr l) 1))
     (t nil)
    )
)

;vedem sa nu crestem din prima
(defun main2(l)
    (cond
        ((> (car l) (car (cdr l))) (vale l 0))
        (t nil)
    )
)

;c
(defun minim(a b)
  (cond
    ((and (not (numberp a)) (not (numberp b))) nil)
    ((not (numberp a)) b)
    ((not (numberp b)) a)
    ((< a b) a)
    (t b)
  )
)

;min_lista minim_lista min_neliniara minim_neliniara
(defun min_lista(l)
    (cond
        ((null l) nil)
        ((AND (equal (lungime l) 1) (atom (car l))) (car l))
        ((listp (car l)) (minim (min_lista (car l)) (min_lista(cdr l))))
        (t (minim (car l) (min_lista (cdr l))))
    )
)

;d elimina_elem_liniara - sterge_elem_liniara
(defun sterge_elem(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) (sterge_elem (cdr l) elem))
        (t (append (list (car l)) (sterge_elem (cdr l) elem)))
    )
)

;max_lista max_neliniara maxim_lista maxim_neliniara
(defun maxim(a b)
  (cond
    ((and (not (numberp a)) (not (numberp b))) nil)
    ((not (numberp a)) b)
    ((not (numberp b)) a)
    ((> a b) a)
    (t b)
  )
)

(defun max_lista(l)
    (cond
        ((null l) nil)

        ;am ajuns la lungimea 1
        ((AND (null (cdr l)) (atom (car l))) (car l))
        ((listp (car l)) (maxim (max_lista (car l)) (max_lista(cdr l))))
        (t (maxim (car l) (max_lista (cdr l))))
    )
)

(defun main3(l)
    (sterge_elem l (max_lista l))
)

;15.
;a) Sa se scrie o functie care intoarce reuniunea a doua multimi.
;b) Sa se construiasca o functie care intoarce produsul atomilor numerici dintr-o lista, de la orice nivel.
;c) Definiti o functie care sorteaza cu pastrarea dublurilor o lista liniara.
;d) Definiti o functie care construiește o listă cu pozițiile elementului minim dintr-o listă liniară numerică.

;a
(defun sterge_prima_aparitie(l elem)
    (cond
        ((null l) nil)
        ((equal (car l) elem) (cdr l))
        (t (append (list (car l)) (sterge_prima_aparitie (cdr l) elem)))
    )
)

(defun reuniune(l1 l2)
    (cond
        ((null l1) l2)
        ((null l2) l1)
        (t (cons (car l1) (reuniune (cdr l1) (sterge_prima_aparitie l2 (car l1)))))
    )
)

;b
(defun produs(l)
    (cond
        ((null l) 1)
        ((listp (car l)) (* (produs (cdr l)) (produs(car l)) ))
        ((numberp (car l)) (* (car l) (produs (cdr l)))) 
        (t (* 1 (produs (cdr l))))
    )
) 

;c 
;(defun minim(a b)
;    (cond
;        ((not (numberp a)) b)
;        ((not (numberp b)) a)
;        ((< a b) a)
;        (t b)
;    )
;)

(defun minim_neliniara(l)
  (cond
    ((null l) nil)
    ((listp (car l)) (minim (minim_neliniara (car l)) (minim_neliniara (cdr l))))
    (t (minim (car l) (minim_neliniara (cdr l))))
  )
)


(defun sort_dubluri(l)
    (cond
        ((null l) nil)
        (t (append (list (minim_neliniara l)) (sort_dubluri (sterge_prima_aparitie l (minim_neliniara l)))))
    )
)

;d
(defun poz_elem(l elem poz)
    (cond
        ((null l) nil)
        ((equal (car l) elem) (append (list poz) (poz_elem (cdr l) elem (+ 1 poz))))
        (t (poz_elem (cdr l) elem (+ 1 poz)))
    )
)

(defun main_poz_elem(l elem)
    (poz_elem l elem 0)
)

(defun poz_min(l)
    (poz_elem l (main_min l))
)
