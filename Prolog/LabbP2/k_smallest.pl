sumofset([],0).

sumofset([X|Rest], Sum):-
    sumofset(Rest, Restn),
    Sum is X + Restn.

shorten([_], []).
shorten([X|Xs], [X | Ys]) :-
    shorten(Xs, Ys).

take(_,[],[]).
take(0,_,[]).
%take(1,[Head|_],[Head]):- !.
take(K,[X|Xs],[X|Ys]) :-
    K > 1,
    K2 is K - 1,
    take(K2,Xs,Ys).

drop(_,[],[]) :- !.
drop(0,L,L) :- !.
drop(1,[_|Tail],Tail) :- !.
drop(K,[_|Tail],Result) :-
    K > 1,
    K2 is K - 1,
    drop(K2,Tail,Result).

subsets([], _, []).
subsets(Lst, Index, Tup):-
   sumofset(Lst, Sum),
   I = Index,
   length(Lst, LNGTH),
   J is I + LNGTH -1,
   FirstTup = [(Sum, I, J, Lst)],
   shorten(Lst, ShorterLst),
   subsets(ShorterLst, I, TupNew),
   append(FirstTup, TupNew, Tup).

getAllSubsets([], _, []).
getAllSubsets([E|T], Start, Sublists):-
    subsets([E|T], Start, Subs),
    NextStart is Start + 1,
    getAllSubsets(T, NextStart, AllSets),
    append(Subs, AllSets, Sublists).

insert((Sum, I, J, Set), [], [(Sum, I, J, Set)]).
insert((Sum, I, J, Set), [(Sum2, I2, J2, Set2)|Lst], [(Sum, I, J, Set), (Sum2, I2, J2, Set2)|Lst]):-
    Sum =< Sum2, !.

insert((Sum, I, J, Set), [(Sum2, I2, J2, Set2)|Lst], [(Sum2, I2, J2, Set2)|Result]):-
    Sum > Sum2,
    insert((Sum, I, J, Set), Lst, Result).

iSort([], []):-!.
iSort((X|Xs), Res):-
    iSort(Xs, TRes),
    insert(X, TRes, Res).

kSmallest([], _, []).
kSmallest(Lst, K, Sublists) :-
   getAllSubsets(Lst, 0, Res),
   iSort(Res, SortedSubs),
   take(K, SortedSubs, Sublists).

stringOutput([(Sum, I, J, Lst)| Rest]) :-
   write(Sum),
   write('\t'),
   write(I),
   write('\t'),
   write(J),
   write('\t'),
   write(Lst),
   write('\n'),
   stringOutput(Rest).


smallestKset(_, 0, _) :-
   write('no sets to pick :,('), !.

smallestKset([], _, _) :-
   write('That is an empty list, how did you tänkte där?'), !.

smallestKset(List, K, Output) :-
   kSmallest(List, K, Output),
   write('Sum\ti\tj\tSublist\n'),
   stringOutput(Output).
























































