% Hjalmar Olofsson Utsi

move(state(SteelKey, BrassKey, Package, Items, r3), drill(r3, r2), state(SteelKey, BrassKey, Package, Items, r2)).
move(state(SteelKey, BrassKey, Package, Items, r2), drill(r2, r3), state(SteelKey, BrassKey, Package, Items, r3)).


%Move from R1 to R3
move(state(SteelKey, holding, Package, Items, r1), walk(r1, r3), state(SteelKey, holding, Package, Items, r3)).

%Move from R3 to R1
move(state(SteelKey, holding, Package, Items, r3), walk(r3, r1), state(SteelKey, holding, Package, Items, r1)).

%Move from R1 to R2
move(state(holding, BrassKey, Package, Items, r1), walk(r1, r2), state(holding, BrassKey, Package, Items, r2)).

%Move from R2 to R1
move(state(holding, BrassKey, Package, Items, r2), walk(r2, r1), state(holding, BrassKey, Package, Items, r1)).

%Grab Steel key
move(state(Room,BrassKey,Package,Items,Room), grab(steelKey,Room), state(holding,BrassKey,Package,Items2,Room)) :-
    Items < 2,
    Items2 is Items + 1.

%Grab Brass key
move(state(SteelKey,Room,Package,Items,Room), grab(brassKey,Room), state(SteelKey,holding,Package,Items2,Room)) :-
    Items < 2,
    Items2 is Items + 1.

%Grab Package
move(state(SteelKey,BrassKey,Room,Items,Room), grab(package,Room), state(SteelKey,BrassKey,holding,Items2,Room)) :-
    Items < 2,
    Items2 is Items + 1.

%Drop Steel key
move(state(holding,BrassKey,Package,Items,Room), drop(steelKey,Room), state(Room,BrassKey,Package,Items2,Room)) :-
    Items > 0,
    Items2 is Items - 1.

%Drop Brass key
move(state(SteelKey,holding,Package,Items,Room), drop(brassKey,Room), state(SteelKey,Room,Package,Items2,Room)) :-
    Items > 0,
    Items2 is Items - 1.

%Drop Package
move(state(SteelKey,BrassKey,holding,Items,Room), drop(package,Room), state(SteelKey,BrassKey,Room,Items2,Room)) :-
    Items > 0,
    Items2 is Items - 1.

solveR(state(_, _, r2, _, _), _,[]).

solveR(State1, N, [Move| Trace2])  :-
    N > 0,
    move(State1, Move, State2),
    N2 is N - 1,
    solveR(State2, N2, Trace2).
