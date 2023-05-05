sumofset([],0).

sumofset([X|Rest], Sum):-
    sumofset(Rest, Restn),
    Sum is X + Restn.

subsets([], ([], _, _)).

subsets([E|Tail], ([E|NTail], I, J)):-
  prefix_subsets(Tail, (NTail, I+1, J)).

subsets([_|Tail], NTail):-
  subsets(Tail,(NTail, I+1, J)).

prefix_subsets(_,([], _, _)).
prefix_subsets([E|Tail], ([E|NTail], I, J)):-
    prefix_subsets(Tail, (NTail, I+1, J)).























































































