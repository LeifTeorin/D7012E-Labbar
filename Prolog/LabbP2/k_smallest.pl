sumofset([],0).

sumofset([X|Rest], Sum):-
    sumofset(Rest, Restn),
    Sum is X + Restn.

take(_,[],[]) :- !.
take(0,L,L) :- !.
take(1,[Head|_],[Head]):- !.
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

subsets([], [], _, _).

subsets([E|Tail], [E|NTail], I, J):-
    length([E|NTail], J),
    NewI is I + 1,
    prefix_subsets(Tail, NTail, NewI, J).

subsets([_|Tail], NTail, I, J):-
    subsets(Tail,NTail, I, J).

prefix_subsets(_,[], _, _).

prefix_subsets([E|Tail], [E|NTail], I, J):-
    length([E|NTail], J),
    NewI is I + 1,
    prefix_subsets(Tail, NTail, NewI, J).

%Creates array of index combinations
list_of_indexes(List,Result) :-
    length(List,Len),
    Lengt is Len - 1,
    findall(Y, between(0,Lengt,Y), PermLst),
    findall(Y, (perm(2,PermLst,X) ,msort(X,Y)), Ys), %create all permutations of that list, and sort it.
    sort(Ys, Result). %sort it again to remove duplicates.

perm(1, Input, [Last]) :-
    member(Last, Input).
perm(N, Input, [First,Second|Perm]) :-
    N > 1, N0 is N-1,
    member(First, Input),
    perm(N0, Input, [Second|Perm]).

subsets_index(List, Subsets):-
    length(List, Length),
    subsets(List, Subsets, 0, Length).

append([],L,L).
append([H|T],L2,[H|L3]) :-
    append(T,L2,L3).

totalSize([],0).
totalSize([sublist(_, Size,_,_)|T],S) :-
    totalSize(T,S2), S is Size+S2.

sizeOf(sublist(_,Size,_,_), Size).

sublist(List, Size, I, J):-
    sumofset(List, Size),
    length(List, Len),
    Len is J - I + 1.










































































