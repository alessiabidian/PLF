% 4.a. Sa se scrie un predicat care substituie intr-o
% lista un element printr-o alta lista.
% b. Sa se elimine elementul de pe pozitia a n-a a unei liste liniare.

% substituie(L: lista initiala, E: element, LS: lista pt substituire,
% R: rezultat)
% (i,i,i,o), (i,i,i,i)

substituie([],_,_,[]).
substituie([H|T],E,LS,[LS|R]):-H is E,!,
	substituie(T,E,LS,R).
substituie([H|T],E,LS,[H|R]):-
	substituie(T,E,LS,R).

% elimina(L: lista originala, N: pozitia, C: contor, R:rezultat)

eliminaPozN([],_,[]).
eliminaPozN([_|T],N,R):-N is 1,
	N1 is 0,
	eliminaPozN(T,N1,R),!.
eliminaPozN([H|T],N,[H|R]):-not(N is 1),
	N1 is N-1,
	eliminaPozN(T,N1,R),!.

% 6.a. Sa se scrie un predicat care elimina dintr-o lista toate
% elementele care se repeta (ex: l=[1,2,1,4,1,3,4] => l=[2,3])
% b. Sa se elimine toate aparitiile elementului maxim dintr-o lista de
% numereintregi.

eliminaAparitii([],_,[]).
eliminaAparitii([H|T],E,R):-
	H=E,!,
	eliminaAparitii(T,E,R).

eliminaAparitii([H|T],E,[H|R]):-
	H\=E,
	eliminaAparitii(T,E,R).

nrAparitii([],_,0).
nrAparitii([H|T],E,N):-
	H=:=E,
	nrAparitii(T,E,NT),
	N is NT+1.
nrAparitii([H|T],E,N):-
	H=\=E,
	nrAparitii(T,E,N).

% eliminaRepetitii(L: lista, R: rezultat)

eliminaRepetitii([],[]).
eliminaRepetitii([H|T],R):-nrAparitii([H|T],H,N),
	N > 1,!,
	eliminaAparitii([H|T],H,LN),
	eliminaRepetitii(LN,R).
eliminaRepetitii([H|T],[H|R]):-eliminaRepetitii(T,R).

% maxim(L: lista, M: maximu rezultat)

maxim([X],X):-!.
maxim([H|T],M):-maxim(T,M1),
	 M is max(H,M1).

% eliminaAparitiiMaxim(L: lista, M: maxim, R: rezultat)

eliminaAparitiiMaxim(L,R):-maxim(L,M),
	eliminaAparitii(L,M,R).

% 7.a. Sa se scrie un predicat care intoarce reuniunea a doua multimi.
% b. Sa se scrie un predicat care, primind o lista, intoarce multimea
% tuturor perechilor din lista. De ex, cu [a, b, c, d] va produce[[a, b],
% [a, c], [a, d], [b, c], [b, d], [c, d]].

% reuniune(L1, L2, R)

construiesteMultime([],[]).
construiesteMultime([H|T],[H|R]):-eliminaAparitii([H|T],H,LN),
	construiesteMultime(LN,R).

% concatenare(L1: lista 1, L2: lista 2, R: rez)
concatenare([],L,L).
concatenare([H|T],L,[H|R]):-concatenare(T,L,R).

reuniune(L1,L2,R):-concatenare(L1,L2,LN),
	construiesteMultime(LN,R).

% faPerechi(L: lista, E: element, R: lista rez cu subliste)

faPerechi([],_,[]).
faPerechi([H|T],E,[[E,H]|R]):-faPerechi(T,E,R).

% listaPerechi(L:lista, R: rez)

listaPerechi([],[]).
listaPerechi([H|T],LN):-faPerechi(T,H,LP),
	listaPerechi(T,R),
	concatenare(LP,R,LN).

% 8.a. Sa se scrie un predicat care testeaza daca o lista este multime.
% b. Sa se scrie un predicat care elimina primele 3 aparitii ale unui
% element intr-o lista. Daca elementul apare mai putin de 3 ori, se va
% elimina de cate ori apare.

verificaMultime([]):-!.
verificaMultime([H|T]):-nrAparitii([H|T],H,N),
	N=1,!,verificaMultime(T).


% eliminaPrimele3Aparitii(L: lista, C: contor, E: element, R: lista
% rezultata)
% (i,i,i), (i,i,o)

eliminaPrimele3Aparitii(L,E,R):-eliminaPrimele3Aparitii(L,E,R,3).
eliminaPrimele3Aparitii([],_,[],_).
eliminaPrimele3Aparitii([H|T],E,R,C):- H is E, C>0,!,
	C1 is C-1,
	eliminaPrimele3Aparitii(T,E,R,C1).
eliminaPrimele3Aparitii([H|T],E,[H|R],C):- eliminaPrimele3Aparitii(T,E,R,C).

% 9.a. Sa se scrie un predicat care intoarce intersectia a doua multimi.
% b. Sa se construiasca lista (m, ..., n), adica multimea numerelor
% intregi din intervalul [m, n].

% intersectieMultimi(L1:lista1, L2: lista2, R:rezultat)
% intersectieMultimi([],[],[]). (i,i,i), (i,i,o).

intersectieMultimi([],_,[]):-!.
intersectieMultimi(_,[],[]):-!.
intersectieMultimi([H|T],L2,[H|R]):-nrAparitii(L2,H,N),
	N=1,!,intersectieMultimi(T,L2,R).
intersectieMultimi([_|T],L2,R):-intersectieMultimi(T,L2,R).


% construireMN(M: limita inf, N: limita sup interval, R: rezultat)

construireMN(M,N,[]):-M>N,!.
construireMN(M,N,[M|R]):-M=<N, M1 is M+1, construireMN(M1,N,R).

% 10.a. Sa se intercaleze un element pe pozitia a n-a a unei liste.
% b. Definiti un predicat care intoarce cel mai mare divizor comun al
% numerelor dintr-o lista.

% interclasarePozN(L: lista, E: elementul, N: pozitia, R: rezultat)

interclasarePozN([],_,_,[]).
interclasarePozN([H|T],E,N,[E|R]):- N=:=1, !, N1 is N-1,
	interclasarePozN([H|T],E,N1,R).
interclasarePozN([H|T],E,N,[H|R]):- N1 is N-1,
	interclasarePozN(T,E,N1,R).

% cmmdc(A: primu nr, B: al 2lea nr, R: rezultat)

cmmdc(A,0,A):-!.
cmmdc(0,B,B):-!.
cmmdc(A,A,A):-!.
cmmdc(A,B,R):-A>B,!,
	B>0,
	A1 is A-B,
	cmmdc(A1,B,R).
cmmdc(A,B,R):-B1 is B-A,
	cmmdc(A,B1,R).

%cmmdc2(X,0,X):-!.
%cmmdc2(0,X,X):-!.
%cmmdc2(X,Y,Z):- X \= 0,Y \= 0,
%    X > Y, X1 is X-Y,
%    cmmdc2(X1,Y,Z),!.
%cmmdc2(X,Y,Z):- X \= 0,Y \= 0,
%    X1 is Y-X,
%    cmmdc2(X,X1,Z),!.

% cmmdcLista(L:lista, C, R)

cmmdcLista([H|T],R):-cmmdcLista(T,H,R).
cmmdcLista([],C,C).
cmmdcLista([H|T],C,R):-cmmdc(H,C,C1),
	cmmdcLista(T,C1,R).

% 11.a. Sa se scrie un predicat care sa testeze daca o lista formata din
% numere intregi are aspect de "vale"(o multime se spune ca are aspect
% de "vale" daca elementele descresc pana la un moment dat, apoi cresc.
% De ex. (10 8 6 9 11 13).
% b. Sa se calculeze suma alternanta a
% elementelor unei liste (l1 -l2 + l3 ...).

% esteVale(L: lista, A: elem anterior, F: flag 1/-1 de crestere/descr)

esteVale([]):-!.
esteVale([H|T]):-esteVale(T,H,0).
esteVale([],_,1):-!.
esteVale([H|T],A,F):-A>H,!, F=<0, esteVale(T,H,-1).
esteVale([H|T],A,F):-A<H, not(F is 0), esteVale(T,H,1).

% sumaAlternanta(L: lista, S: rez)

sumaAlternanta([],0).
sumaAlternanta([X],X):-!.
sumaAlternanta([Cap|Coada],X):-sumaAlternanta(Coada,X1),
    X is Cap-X1.

% 12.a. Sa se scrie un predicat care substituie intr-o lista un element
% prin altul.
% b. Sa se construiasca sublista (lm, ..., ln) a listei (l1,
% ..., lk).

% substituie(L: lista, E: elem, N: elem nou, R: lista rezultata)

substituie12([],_,_,[]).
substituie12([H|T],E,N,[N|R]):-H is E,!,
	substituie12(T,E,N,R).
substituie12([H|T],E,N,[H|R]):-substituie12(T,E,N,R).


startConstrSublista(L,M,N,R):-constrSublista(L,1,M,N,R).

constrSublista([],_,_,_,[]).
constrSublista([H|T],P,M,N,[H|R]):-P>=M, P=<N,!,
	P1 is P+1,
	constrSublista(T,P1,M,N,R).
constrSublista([_|T],P,M,N,R):-	P1 is P+1,
	constrSublista(T,P1,M,N,R).

%13.a. Sa se scrie un predicat care transforma o lista intr-o multime, in ordinea ultimei aparitii. Exemplu: [1,2,3,1,2] e transformat in [3,1,2].
% b. Sa se calculeze cel mai mare divizor comun al elementelor unei liste.

multimeUltimaAp([],[]).
multimeUltimaAp([H|T],[H|R]):-nrAparitii([H|T],H,N),
	N=:=1,!,
	multimeUltimaAp(T,R).
multimeUltimaAp([_|T],R):-multimeUltimaAp(T,R).

% 14.a. Sa se scrie un predicat care testeaza egalitatea a doua multimi,
% fara sa se faca apel la diferenta a doua multimi.
% b. Definiti un predicat care selecteaza al n-lea element al unei liste.

% egalitateMultimi(L1, L2)

% ------------------- NU NU
startEgalitateMultimi(L1,L2):-egalitateMultimi(L1,L2), egalitateMultimi(L2,L1).

egalitateMultimi([],_).
egalitateMultimi([H|T],L):-nrAparitii(L,H,N),
	N is 1,
	egalitateMultimi(T,L).
% ------------------- NU NU

selecteazaN([H|_],1,H):-!.
selecteazaN([_|T],N,R):-N>1,
	N1 is N-1,
	selecteazaN(T,N1,R).

% 15.a. Sa se scrie un predicat care se va satisface daca o lista are numar par de elemente si va esua in caz contrar, fara sa se numere elementele listei.
% b. Sa se elimine prima aparitie a elementului minim dintr-o lista de
% numereintregi.

% invers(L: lista, R: rezultat)
% (i,i), (i,o)
invers(L, Rez) :- invers_aux([], L, Rez).

invers_aux(NewL, [], NewL).
invers_aux(NewL, [H|T], Rez) :-
    invers_aux([H|NewL], T, Rez),!.

% IDKK
nrParElem([],[],_,0).
nrParElem([_|T1],[_|T2],P1,P2):-P11 is P1+1,
	nrParElem(T1,T2,P11,P22),
	P2 = P22+1,
	P1=\=P2.

verificaNrPar(L):-invers(L,LI),
	nrParElem(L,LI,1,s).


minim([E],E):-!.
minim([H|T],M):-minim(T,M1),
	M is min(H,M1).

eliminaPrimaAparitie([],_,_,[]).
eliminaPrimaAparitie([H|T],E,OK,R):-H=:=E,
	OK=:=0,!,
	eliminaPrimaAparitie(T,E,1,R).
eliminaPrimaAparitie([H|T],E,OK,[H|R]):-eliminaPrimaAparitie(T,E,OK,R).

elimPrimaApMin(L,R):-minim(L,M),
	eliminaPrimaAparitie(L,M,0,R).
