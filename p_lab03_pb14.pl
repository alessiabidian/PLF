/*14. Sa se scrie un program care genereaza lista submultimilor de suma S data, cu elementele  unei  liste.
Ex:[1,2,3,4,5,6,10]  si  S=10  =>  [[1,2,3,4],  [1,4,5], [2,3,5], [4,6], [10]](nu neaparat in aceasta ordine)*/

% submultimi(L: lista mare, R: sublista)
% (i,o), (i,i)
submultimi([],[]).
submultimi([_|T],Rez):-submultimi(T,Rez).
submultimi([H|T],[H|Rez]):-submultimi(T,Rez).

% suma(L: lista, S: suma elem din lista)
% (i,i), (i,o)
suma([],0).
suma([H|T],S):-suma(T,S1),S is S1 + H.

/*
conditie_submultimi_suma(L: lista data, S: suma, R: rezultat posibil)
submultimi_suma(L: lista data, S: suma, R: lista cu toate rezultatele posibile
(i,i,o), (i,i,i)
*/
conditie_submultimi_suma(L,S,Rez):-submultimi(L,Rez),suma(Rez,S).

submultimi_suma(L,S,Rez):-findall(Colector,conditie_submultimi_suma(L,S,Colector),Rez).
