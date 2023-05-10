%Move from R1 to R3
move(state(SK, has, Pa, Items, r1), walk(r1, r3), state(SK, has, Pa, Items, r3)).

%Move from R3 to R1
move(state(SK, has, Pa, Items, r3), walk(r3, r1), state(SK, has, Pa, Items, r1)).

%Move from R1 to R2 
move(state(has, BK, Pa, Items, r1), walk(r1, r2), state(has, BK, Pa, Items, r2)).

%Move from R2 to R1
move(state(has, BK, Pa, Items, r2), walk(r2, r1), state(has, BK, Pa, Items, r1)).

%Grab Steel key
move(state(Room,BK,Pa,Items,Room), grasp(steelKey,Room), state(has,BK,Pa,Items2,Room)) :- 
    Items < 2, 
    Items2 is Items + 1.

%Grab Brass key
move(state(SK,Room,Pa,Items,Room), grasp(brassKey,Room), state(SK,has,Pa,Items2,Room)) :- 
    Items < 2, 
    Items2 is Items + 1.

%Grab Package
move(state(SK,BK,Room,Items,Room), grasp(package,Room), state(SK,BK,has,Items2,Room)) :- 
    Items < 2, 
    Items2 is Items + 1.

%Drop Steel key
move(state(has,BK,Pa,Items,Room), drop(steelKey,Room), state(SK,BK,Pa,Items2,Room)) :- 
    Items > 0, 
    Items2 is Items - 1.

%Drop Brass key
move(state(SK,has,Pa,Items,Room), drop(brassKey,Room), state(SK,BK,Pa,Items2,Room)) :- 
    Items > 0, 
    Items2 is Items - 1.

%Drop Package
move(state(SK,BK,has,Items,Room), drop(package,Room), state(SK,BK,Pa,Items2,Room)) :- 
    Items > 0, 
    Items2 is Items - 1.

solveR(state(_, _r2, _, r2), _,[done| []]).

solveR(State1, N, [Move| Trace2])  :-
    N > 0,
    move(State1, Move, State2),
    N2 is N - 1,
    solveR(State2, N2, Trace2). 