% 1.a)Definiti un predicat care determina suma a doua numere
% scrise in reprezentare de lista.
% b)Se da o lista eterogena, formata din numere intregi si liste de
% cifre. Sa se calculeze suma tuturor numerelor reprezentate de subliste.
% De ex:[1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>[8, 2, 2].

% -------------a)
invers(L, Rez) :- invers_aux([], L, Rez).

invers_aux(NewL, [], NewL).
invers_aux(NewL, [H|T], Rez) :-
    invers_aux([H|NewL], T, Rez),!.

% suma2NrListe(L1: lista1, L2: lista2, C:carry, R: lista rezultat)
% (i,i,i,o), (i,i,i,i)
suma2NrListe(L1,L2,R):-invers(L1,L1I),
	invers(L2,L2I),
	suma2NrListe_sec(L1I,L2I,0,RI),
	invers(RI,R).

% suma2NrListe_sec(L1: lista1, L2: lista2, C: carry, R: lista rezultat)
% (i,i,i,o), (i,i,i,i)
suma2NrListe_sec([],[],0,[]):-!.
suma2NrListe_sec([],[],C,[C]):-!.
suma2NrListe_sec([H1|T1],[H2|T2],C,[R1|R]):-!,R1 is (H1+H2+C) mod 10,
	C1 is (H1+H2+C) div 10,
	suma2NrListe_sec(T1,T2,C1,R).
suma2NrListe_sec([H1|T1],[],C,[R1|R]):-!,R1 is (H1+C) mod 10,
	C1 is (H1+C) div 10,
	suma2NrListe_sec(T1,[],C1,R).
suma2NrListe_sec([],[H2|T2],C,[R1|R]):-!,R1 is (H2+C) mod 10,
	C1 is (H2+C) div 10,
	suma2NrListe_sec(T2,[],C1,R).


sumeSubliste([],[0]).
sumeSubliste([H|T],S):-is_list(H),!,
	   sumeSubliste(T,R),
	   suma2NrListe(H,R,S).
sumeSubliste([_|T],S):-sumeSubliste(T,S).

% 2.  a)Sa se sorteze o lista cu pastrarea dublurilor. De ex: [4 2 6 2 3 4] => [2 2 3 4 4 6]
% b)Se da o lista eterogena, formata din numereintregi si liste de
% numere. Sa se sorteze fiecare sublista cu pastrareadublurilor. De
% ex:[1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>[1, 2,
% [1, 4, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1, 1, 1], 7].

combinaListe([],Rez,Rez).
combinaListe([H1|L1],L2,[H1|Rez]):-combinaListe(L1,L2,Rez).

sortare([],[]).
sortare([H|T],Rez):-sortare_aux(H,T,[],[],Rez).

sortare_aux(X,[H|T],L1,L2,Rez):-H < X,!,sortare_aux(X,T,[H|L1],L2,Rez).
sortare_aux(X,[H|T],L1,L2,Rez):-H >= X,!,sortare_aux(X,T,L1,[H|L2],Rez).
sortare_aux(X,[],L1,L2,Rez):-sortare(L1,L1X),sortare(L2,L2X),combinaListe(L1X,[X|L2X],Rez).

sortareSubliste([],[]).
sortareSubliste([H|T],[H1|R]):-is_list(H),!,
	sortare(H,H1),
	sortareSubliste(T,R).
sortareSubliste([H|T],[H|R]):-sortareSubliste(T,R).

% 3.a)Sa se sorteze o lista cu eliminarea dublurilor. De ex: [4 2 6 2 3 4] =>[2 3 4 6]
% b)Se da o lista eterogena, formata din numere intregi si liste de
% numere. Sa se sorteze fiecare sublista fara pastrarea dublurilor. De
% ex:[1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>[1, 2,
% [1, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1], 7].

sortareElim([],[]).
sortareElim([H|T],Rez):-sortareElim_aux(H,T,[],[],Rez).

sortareElim_aux(X,[H|T],L1,L2,Rez):-H < X,!,sortareElim_aux(X,T,[H|L1],L2,Rez).
sortareElim_aux(X,[H|T],L1,L2,Rez):-H > X,!,sortareElim_aux(X,T,L1,[H|L2],Rez).
sortareElim_aux(X,[H|T],L1,L2,Rez):-H = X,!,sortareElim_aux(X,T,L1,L2,Rez).
sortareElim_aux(X,[],L1,L2,Rez):-sortareElim(L1,L1X),sortareElim(L2,L2X),combinaListe(L1X,[X|L2X],Rez).

sublistaSortElim([],[]).
sublistaSortElim([H|T],[H1|R]):-is_list(H),!,
	sortareElim(H,H1),
	sublistaSortElim(T,R).
sublistaSortElim([H|T],[H|R]):-sublistaSortElim(T,R).

% 4.a)Sa se interclaseze fara pastrarea dublurilor doua liste sortate.
% b)Se da o lista eterogena, formata din numere intregi si liste de
% numere sortate. Sa se interclaseze fara pastrarea dublurilor toate
% sublistele. De ex:[1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5,
% [1, 1, 11], 8] =>[1, 2, 3, 4, 6, 7, 9, 10, 11].

interclasare_sec(L1,[],L1):-!.
interclasare_sec([],L2,L2):-!.
interclasare_sec([H1|T1],[H2|T2],[H1|R]):-H1<H2,!,
	interclasare_sec(T1,[H2|T2],R).
interclasare_sec([H1|T1],[H2|T2],[H2|R]):-H1>H2,!,
	interclasare_sec([H1|T1],T2,R).
interclasare_sec([H1|T1],[H2|T2],R):-H1=:=H2,
	interclasare_sec([H1|T1],T2,R).

unicate([],[]):-!.
unicate([E],[E]):-!.
unicate([H|T],R):-unicate_aux(T,H,R).

unicate_aux([],E,[E]).
unicate_aux([H|T],Anterior,[Anterior|R]):-Anterior=\=H,!,
	unicate_aux(T,H,R).
unicate_aux([H|T],H,R):-unicate_aux(T,H,R).

interclasare(L1,L2,R):-unicate(L1,R1),
	unicate(L2,R2),
	interclasare_sec(R1,R2,R).

% ----b)
interclasareSubliste([],[]).
interclasareSubliste([H|T],R):-is_list(H),!,
	interclasareSubliste(T,R1),
	interclasare(H,R1,R).
interclasareSubliste([_|T],R):-interclasareSubliste(T,R).


% 5.a)Sa se determine pozitiile elementului maxim dintr-o lista liniara.
% De ex:poz([10,14,12,13,14], L) va produce L = [2,5].
% b)Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Sa se inlocuiasca fiecare sublista cu pozitiile
% elementului maxim din sublista respectiva. De ex:[1, [2, 3], [4, 1,
% 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>[1, [2], [1, 3], 3, 6,
% [2], 5, [1, 2, 3], 7]


maxim([E],E):-!.
maxim([H|T],M):-maxim(T,R),
	M is max(H,R).

determinaPoz([],_,_,[]).
determinaPoz([H|T],E,P,[P|R]):-H is E,!,
	P1 is P+1,
	determinaPoz(T,E,P1,R).
determinaPoz([_|T],E,P,R):-P1 is P+1, determinaPoz(T,E,P1,R).

determinaPozMax(L,R):-maxim(L,M), determinaPoz(L,M,1,R).


inlocuireCuPozMax([],[]).
inlocuireCuPozMax([H|T],[R1|R]):-is_list(H),!,
	determinaPozMax(H,R1),
	inlocuireCuPozMax(T,R).
inlocuireCuPozMax([H|T],[H|R]):-inlocuireCuPozMax(T,R).

%6.a)Intr-o lista L sa se inlocuiasca toate aparitiile unui element E cu
% elementele unei alte liste, L1. De ex:
% inloc([1,2,1,3,1,4],1,[10,11],X)va produce X=[10,11,2,10,11,3,10,11,4].
% b)Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. In fiecare sublista sa se inlocuiasca toate aparitiile
% primului element din sublista cu o lista data. De ex:[1, [4, 1, 4], 3,
% 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] si [11, 11] =>[1, [11, 11, 1, 11,
% 11], 3, 6, [11, 11, 10, 1, 3, 9], 5, [11 11 11 11 11 11], 7]

inlocElemCuElemSublista([],_,_,[]).
inlocElemCuElemSublista([H|T],E,L,R1):-H is E,!,
	        inlocElemCuElemSublista(T,E,L,R),
		combinaListe(L,R,R1).
inlocElemCuElemSublista([H|T],E,L,[H|R]):-
	inlocElemCuElemSublista(T,E,L,R).


primulElem([H|_],H).


inlocSubliste6([],_,[]).
inlocSubliste6([H|T],L,[R1|R]):-is_list(H),
	primulElem(H,H1),!,
	inlocElemCuElemSublista(H,H1,L,R1),
	inlocSubliste6(T,L,R).
inlocSubliste6([H|T],L,[H|R]):-inlocSubliste6(T,L,R).

% 8.a)Definiti un predicat care determina succesorul unui numar
% reprezentat cifra cu cifra intr-o lista.
% De ex: [1 9 3 5 9 9] => [1 9 3 6 0 0]
% b)Se da o lista eterogena, formata din numere intregi si
% liste de cifre. Pentru fiecare sublista sa se determine succesorul
% numarului reprezentat cifra cu cifra de lista respectiva.
% De ex: [1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] => [1, [2,
% 4], 4, 5, [6, 8, 0], 10, 11, [1, 2, 1], 6]


% suma2NrListe(L1: lista1, L2: lista2, C:carry, R: lista rezultat)
% (i,i,i,o), (i,i,i,i)
succesor(L,R):-invers(L,LI),
	succesor_aux(LI,1,RI),
	invers(RI,R).

% succesor(L: lista, C: carry, R: lista rezultat)
% (i,i,o), (i,i,i)
succesor_aux([],0,[]):-!.
succesor_aux([],C,[C]):-!.
succesor_aux([H|T],C,[R1|R]):-!,R1 is (H+C) mod 10,
	C1 is (H+C) div 10,
	succesor_aux(T,C1,R).


succesorSubliste([],[]).
succesorSubliste([H|T],[S|R]):-is_list(H),!,
	succesor(H,S),
	succesorSubliste(T,R).
succesorSubliste([H|T],[H|R]):-succesorSubliste(T,R).

% 9. a)Dandu-se o lista liniara numerica, sa  se  stearga
% toate  secventele  de valori consecutive.
% Ex: sterg([1, 2, 4, 6, 7, 8, 10], L) va produce L=[4, 10].
% b)Se da o lista eterogena, formata din numere intregi si liste de
% numere intregi. Din fiecare sublista sa se stearga toate secventele de
% valori consecutive. De ex:[1, [2, 3, 5], 9, [1, 2, 4, 3, 4, 5, 7, 9],
% 11, [5, 8, 2], 7] =>[1, [5], 9, [4, 7, 9], 11, [5, 8, 2], 7]

gasesteSecvConsec([],_,_,[]).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == 1,
	H1 is H-1,
	Anterior == H1,!,
	gasesteSecvConsec(T,OK,H,R).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == 0,
	H1 is H-1,
	Anterior == H1,!,
	gasesteSecvConsec(T,1,H,R).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == -1,
	H1 is H+1,
	Anterior == H1,!,
	gasesteSecvConsec(T,OK,H,R).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == 0,
	H1 is H+1,
	Anterior == H1,!,
	gasesteSecvConsec(T,-1,H,R).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == 1,
	H1 is H-1,
	Anterior =\= H1,!,
	gasesteSecvConsec(T,0,H,R).
gasesteSecvConsec([H|T],OK,Anterior,[Anterior|R]):-
	OK == -1,
	H1 is H+1,
	Anterior =\= H1,!,
	gasesteSecvConsec(T,0,H,R).
gasesteSecvConsec([H|T],OK,_,R):-
	OK == 0,
        gasesteSecvConsec(T,0,H,R).


apareElem([E|_],E):-!.
apareElem([H|T],E):-H =\= E,!, apareElem(T,E).

sterg([],[]):-!.
sterg([E],[E]):-!.
sterg([H|T],Rez):-sterg_aux(T,H,RezX),sterg_f([H|RezX],Rez).
/*
sterg_f([],[]):-!.
sterg_f([e],[]):-!.
sterg_f([e|T],Rez):-sterg_f(T,Rez),!.
sterg_f([_|[e|T]],Rez):-sterg_f(T,Rez),!.
sterg_f([H|T],[H|Rez]):-sterg_f(T,Rez).

sterg_aux([H],P,[]):-H is P+1,!.
sterg_aux([H],_,[H]):-!.
sterg_aux([H|T],P,[e|Rez]):-H is P+1,!,
    sterg_aux(T,H,Rez).
sterg_aux([H|T],_,[H|Rez]):-sterg_aux(T,H,Rez).*/



% 10. a)Se da o lista de numere intregi.
% Se cere sa se adauge in lista dupa 1-ul element, al 3-lea element, al
% 7-lea elemen, al 15-lea element ... o valoare data E.
% b)Se da o lista eterogena, formata din numere intregi si liste de numere
% intregi. Lista incepe cu un numar si nu sunt 2 elemente consecutive care sunt liste.
% Se cere ca in fiecare sublista sa se adauge dupa 1-ul, al 3-lea, al
% 7-lea... element valoarea care se gaseste inainte de sublista in lista
% eterogena. De ex:[1, [2, 3], 7, [4, 1, 4], 3, 6, [7, 5, 1, 3, 9, 8, 2,
% 7], 5] =>[1, [2, 1, 3], 7, [4, 7, 1, 4, 7], 3, 6, [7, 6, 5, 1, 6, 3, 9,
% 8, 2, 6, 7], 5].

insereazaDupa([],E,P,P,[E]):-!.
insereazaDupa([],_,_,_,[]).
insereazaDupa([H|T],E,P,I,[E,H|R]):- I==P,!,
	P1 is P*2,
	I1 is I+1,
	insereazaDupa(T,E,P1,I1,R).
insereazaDupa([H|T],E,P,I,[H|R]):-I1 is I+1,
	insereazaDupa(T,E,P,I1,R).


insereazaDupa_start(L,E,R):-insereazaDupa(L,E,2,1,R).


insereazaDupa_Subliste([],_,[]).
insereazaDupa_Subliste([H|T],Anterior,[R1|R]):-is_list(H),!,
	insereazaDupa_start(H,Anterior,R1),
	insereazaDupa_Subliste(T,H,R).
insereazaDupa_Subliste([H|T],_,[H|R]):-
	insereazaDupa_Subliste(T,H,R).

insereaza([H|T],[H|R]):-insereazaDupa_Subliste(T,H,R).

% 11. a)Se da o lista de numere intregi. Se cere sa se scrie de 2 ori in
% lista fiecare numar prim.
% b)Se da o lista eterogena, formata din
% numere intregi si liste de numere intregi. Se cere ca in fiecare
% sublista sa se scrie de 2 ori fiecare numar prim. De ex:[1, [2, 3], 4,
% 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5] =>[1, [2, 2, 3, 3], 4, 5, [1, 4,
% 6], 3, [1, 3, 3, 7, 7, 9, 10], 5]

isNrPrim(E):-isNrPrim(E,2).
isNrPrim(E,E):-!.
isNrPrim(E,D):- D < E,
	E mod D =\= 0,
	D1 is D+1,
	isNrPrim(E,D1).

dubluriPrim([],[]).
dubluriPrim([H|T],[H,H|R]):-isNrPrim(H),!,
	dubluriPrim(T,R).
dubluriPrim([H|T],[H|R]):-dubluriPrim(T,R).

dubluriPrimSubliste([],[]).
dubluriPrimSubliste([H|T],[R1|R]):-is_list(H),!,
	dubluriPrim(H,R1),
	dubluriPrimSubliste(T,R).
dubluriPrimSubliste([H|T],[H|R]):-dubluriPrimSubliste(T,R).

/*12. a)Sa se inlocuiasca toate aparitiile unui element dintr-o
lista cu un alt element.
b)Se da o lista eterogena,formata din nr intregi si liste de numere
intregi. Se cere ca toate aparitiile elementului maxim (dintre valorile
intregi ale listei) sa fie inlocuite in fiecare sublista cu maximul
sublistei respective. De ex: [1, [2, 5, 7], 4, 5, [1, 4], 3, [1, 3, 5,
8, 5, 4], 5, [5, 9, 1], 2] =>[1, [2, 7, 7], 4, 5, [1, 4], 3, [1, 3, 8,
8, 8, 4], 5, [9, 9, 1], 2]
*/

inlocuiesteE([],_,_,[]).
inlocuiesteE([H|T],E1,E2,[E2|R]):-H is E1,!,
	inlocuiesteE(T,E1,E2,R).
inlocuiesteE([H|T],E1,E2,[H|R]):-
	inlocuiesteE(T,E1,E2,R).

elemMaxim(L,R):-elemMaxim(L,0,R).
elemMaxim([],Last,[Last]).
elemMaxim([H|T],_,M):-not(is_list(H)),!,
	elemMaxim(T,H,R),
	M is max(H,R).
elemMaxim([_|T],Last,R):-
	elemMaxim(T,Last,R).

inlocuiesteMax([],_,[]).
inlocuiesteMax([H|T],M,[R1|R]):-is_list(H),!,
	elemMaxim(H,M1),
	inlocuiesteE(H,M,M1,R1),
	inlocuiesteMax(T,M,R).
inlocuiesteMax([H|T],M,[H|R]):-
	inlocuiesteMax(T,M,R).

inlocuiesteMax_start(L,R):-elemMaxim(L,M),
	inlocuiesteMax(L,M,R).

/*13.a)Sa se adauge dupa fiecare element dintr-o lista divizorii elementului.
b)Se  da  o  lista  eterogena,  formata  din  numere  intregi  si  liste de  numere intregi. Se cere ca in fiecare sublista sa se adauge dupa fiecare element divizorii elementului. De ex:[1, [2, 5, 7], 4, 5, [1, 4], 3, 2, [6, 2, 1], 4, [7, 2, 8, 1], 2]=>[1, [2, 5, 7], 4, 5, [1, 4, 2], 3, 2, [6, 2, 3, 2, 1], 4, [7, 2, 8, 2, 4, 1], 2]*/

divizori_start(E,R):-divizori(E,R,2).
divizori(E,[],E):-!.
divizori(E,[D|R],D):-D < E,
	(E mod D) =:= 0,!,
	D1 is D+1,
	divizori(E,R,D1).
divizori(E,R,D):-D < E,
	D1 is D+1,
	divizori(E,R,D1).


adaugaDivizori([],[]).
adaugaDivizori([H|T],[H|R1]):-divizori_start(H,L),
	adaugaDivizori(T,R),
	combinaListe(L,R,R1).


/*14.a)Definiti un predicat care determina predecesorul unui numar reprezentatcifra cu cifra intr-o lista. De ex: [1 9 3 6 0 0] => [1 9 3 5 9 9]
b)Se  da  o  lista  eterogena,  formata  din  numere  intregi  si  liste  de  cifre. Pentru fiecare sublista sa se determine predecesorul numarului reprezentat cifra cucifra de lista respectiva. De ex:[1, [2, 3], 4, 5, [6, 7, 9], 10, 11, [1, 2, 0], 6] =>[1, [2, 2], 4, 5, [6, 7, 8], 10, 11, [1, 1, 9] 6]*/

predecesor(L,R):-invers(L,LI),
	predecesor_aux(LI,-1,RI),
	invers(RI,R).

predecesor_aux([],0,[]):-!.
predecesor_aux([],C,[C]):-!.
predecesor_aux([H|T],C,[R1|R]):-!,R1 is (H+C) mod 10,
	C1 is (H+C) div 10,
	predecesor_aux(T,C1,R).

/*15.a)Sa se determine cea mai lunga secventa de numere pare consecutive dintr-o lista (daca sunt mai multe secvente delungime maxima, una dintre ele).
b)Se  da  o  lista  eterogena,  formata  din  numere  intregi  si  liste  de  numere intregi. Sa se inlocuiasca fiecare sublista cu cea mai lunga secventa de numere pare consecutive din sublista respectiva. De ex:[1, [2, 1, 4, 6, 7], 5, [1, 2, 3, 4], 2, [1, 4, 6, 8, 3], 2, [1, 5], 3] =>[1, [4, 6], 5, [2], 2, [4, 6, 8], 2, [], 3]*/

mai_mare([],[]):-!.
mai_mare([_|_],[]):-!.
mai_mare([_|T1],[_|T2]):-mai_mare(T1,T2).

secv_pare([H|T],[H|Rez]):- H mod 2 =:= 0,!,secv_pare(T,Rez).
secv_pare(_,[]).

secv_compar([],[]).
secv_compar([H|T],Pare):-
	secv_compar(T,Rez),
	secv_pare([H|T],Pare),
	mai_mare(Pare,Rez),!.
secv_compar([_|T],Rez):-
	secv_compar(T,Rez).
