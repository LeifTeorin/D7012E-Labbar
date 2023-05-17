% Hjalmar Olofsson Utsi

sumofset([],0).
sumofset([X|Rest], Sum):-
    sumofset(Rest, Restn),
    Sum is X + Restn.

removeFirst([_], []).
removeFirst([X|Xs], [X | Ys]) :-
    removeFirst(Xs, Ys).

take(_,[],[]).
take(0,_,[]).
take(K,[X|Xs],[X|Ys]) :-
    K2 is K - 1,
    take(K2,Xs,Ys).

subsets([], _, []).
subsets(Lst, Index, Tup):-
   sumofset(Lst, Sum),
   I = Index,
   length(Lst, LENGTH),
   J is I + LENGTH - 1,
   FirstTup = [(Sum, I, J, Lst)],
   removeFirst(Lst, NewLst),
   subsets(NewLst, I, TupNew),
   append(FirstTup, TupNew, Tup).

getAllSubsets([], _, []).
getAllSubsets([E|T], Start, Sublists):-
    subsets([E|T], Start, Subs),
    NextStart is Start + 1,
    getAllSubsets(T, NextStart, AllSets),
    append(Subs, AllSets, Sublists).

insert((Sum, I, J, Set), [], [(Sum, I, J, Set)]).
insert((Sum, I, J, Set), [(Sum2, I2, J2, Set2)|Lst], [(Sum, I, J, Set), (Sum2, I2, J2, Set2)|Lst]):-
    Sum =< Sum2.

insert((Sum, I, J, Set), [(Sum2, I2, J2, Set2)|Lst], [(Sum2, I2, J2, Set2)|Result]):-
    Sum > Sum2,
    insert((Sum, I, J, Set), Lst, Result).

iSort([], []).
iSort([(Sum, I, J, Set)|Xs], Res):-
    iSort(Xs, TempRes),
    insert((Sum, I, J, Set), TempRes, Res).

kSmallest([], _, []).
kSmallest(Lst, K, Sublists) :-
   getAllSubsets(Lst, 0, Res),
   iSort(Res, Sorted),
   take(K, Sorted, Sublists).

writeKSmallest([(Sum, I, J, Lst)| Rest]) :-
   write(Sum),
   write('\t'),
   write(I),
   write('\t'),
   write(J),
   write('\t'),
   write(Lst),
   write('\n'),
   writeKSmallest(Rest).


smallestKset(_, 0, _) :-
   write('no sets to pick :,('), !.

smallestKset([], _, _) :-
   write('That is an empty list, how did you t�nkte d�r?'), !.

smallestKset(List, K, Output) :-
   kSmallest(List, K, Output),
   write('Sum\ti\tj\tSublist\tk = '),
   write(K),
   write('\n'),
   writeKSmallest(Output).
























































