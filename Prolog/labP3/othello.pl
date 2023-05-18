/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING>
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING>
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them.
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr)
%          * tie(State)
%          * terminal(State)
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation:
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows:
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position.





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.],
            [.,.,.,.,.,.],
		[.,.,1,2,.,.],
		[.,.,2,1,.,.],
            [.,.,.,.,.,.],
		[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr).
%%%  holds if InitialState is the initial state and
%%%  InitialPlyr is the player who moves first.

initialize(InitialState, 1):-
	initBoard(InitialState).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player

countRow([], _, 0).
countRow([Head|Row], Plyr, Score):-
	Head = Plyr,
	countRow(Row, Plyr, Score2),
	Score = Score2 + 1.

countRow([Head|Row], Plyr, Score):-
	Head \= Plyr,
	countRow(Row, Plyr, Score).

score([], _, 0).
score([Row|State], Plyr, Score):-
	score(State, Plyr, RestScore),
	countRow(Row, Plyr, RowScore),
	Score = RestScore + RowScore.

winner(State, Plyr):-
	terminal(State),
	score(State, 1, Score1),
	score(State, 2, Score2),
	((Score1>Score2, Plyr = 1) ; (Score1<Score2, Plyr = 2)).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here.
%    - true if terminal State is a "tie" (no winner)

tie(State):-
	terminal(State),
	score(State, 1, Score1),
	score(State, 2, Score2),
	Score1 = Score2.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State).
%   - true if State is a terminal

terminal(State):-
	moves(1, State, Plyr1Moves), !,
	Plyr1Moves == [n],
	moves(2, State, Plyr2Moves), !,
	Plyr2Moves == [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. Its used by play.pl
%%

showState( G ) :-
	printRows( G ).

printRows( [] ).
printRows( [H|L] ) :-
	printList(H),
	nl,
	printRows(L).

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define moves(Plyr,State,MvList).
%   - returns list MvList of all legal moves Plyr can make in State
%

getPlayerPos(_, [], _, _, []).
getPlayerPositions(Plyr, [Head | RestOfRow], X, Y, PlayerPositions) :-
	Plyr = Head,
	PlayerCoords = [[X, Y]],
	NextX is X + 1,
	getPlayerPositions(Plyr, RestOfRow, NextX, Y, NewCoords),
	append(PlayerCoords, NewCoords, PlayerPositions).

getAllPlayerPos(_, [], _, []).
getAllPlayerPos(Plyr, [FirstRow | RestOfRows], Y, Positions):-
	getPlayerPositions(Plyr, FirstRow, 0, Y, PlayerRowPositions),
	NextRow is Y + 1,
	getAllPlayerPos(Plyr, RestOfRows, NextRow, RestRowPositions),
	append(PlayerRowPositions, RestRowPositions, Positions).

moves(Plyr, State, MvList):-
	Plyr = 2,
	getAllPlayerPos(Plyr, State, 0, Positions),
	movesNorth(Plyr, 1, State, Positions, NorthMoves),
	movesSouth(Plyr, 1, State, Positions, SouthMoves),
	movesWest(Plyr, 1, State, Positions, WestMoves),
	movesEast(Plyr, 1, State, Positions, EastMoves),
	movesNE(Plyr, 1, State, Positions, NEMoves),
	movesSE(Plyr, 1, State, Positions, SEMoves),
	movesNW(Plyr, 1, State, Positions, NWMoves),
	movesSW(Plyr, 1, State, Positions, SWMoves),
	append(NorthMoves, SouthMoves, NorthSouth),
	append(NorthSouth, WestMoves, MovesNSW),
	append(MovesNSW, EastMoves, MovesWESN),
	append(MovesWESN, NEMoves, MovesWESNNE),
	append(MovesWESNNE, SEMoves, MovesWESNNESE),
	append(MovesWESNNESE, NWMoves, MovesWESNNESENW),
	append(MovesWESNNESENW, SWMoves, MVListUnsorted),
	sort(MVListUnsorted, MvList).

moves(Plyr, State, MvList):-
	Plyr = 1,
	getAllPlayerPos(Plyr, State, 0, Positions),
	movesNorth(Plyr, 2, State, Positions, NorthMoves),
	movesSouth(Plyr, 2, State, Positions, SouthMoves),
	movesWest(Plyr, 2, State, Positions, WestMoves),
	movesEast(Plyr, 2, State, Positions, EastMoves),
	movesNE(Plyr, 2, State, Positions, NEMoves),
	movesSE(Plyr, 2, State, Positions, SEMoves),
	movesNW(Plyr, 2, State, Positions, NWMoves),
	movesSW(Plyr, 2, State, Positions, SWMoves),
	append(NorthMoves, SouthMoves, NorthSouth),
	append(NorthSouth, WestMoves, MovesNSW),
	append(MovesNSW, EastMoves, MovesWESN),
	append(MovesWESN, NEMoves, MovesWESNNE),
	append(MovesWESNNE, SEMoves, MovesWESNNESE),
	append(MovesWESNNESE, NWMoves, MovesWESNNESENW),
	append(MovesWESNNESENW, SWMoves, MVListUnsorted),
	sort(MVListUnsorted, MvList).

movesNorth(_, _, _, [], []).
movesNorth(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNorth(Plyr, Enemy, State, X, Y, valid),
	movesNorth(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesNorth(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNorth(Plyr, Enemy, State, X, Y, notvalid),
	movesNorth(Plyr, Enemy, State, RestMoves, Moves).


movesSouth(_, _, _, [], []).
movesSouth(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSouth(Plyr, Enemy, State, X, Y, valid),
	movesSouth(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesSouth(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSouth(Plyr, Enemy, State, X, Y, notvalid),
	movesSouth(Plyr, Enemy, State, RestMoves, Moves).

movesWest(_, _, _, [], []).
movesWest(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkWest(Plyr, Enemy, State, X, Y, valid),
	movesWest(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesWest(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkWest(Plyr, Enemy, State, X, Y, notvalid),
	movesWest(Plyr, Enemy, State, RestMoves, Moves).

movesEast(_, _, _, [], []).
movesEast(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkEast(Plyr, Enemy, State, X, Y, valid),
	movesEast(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesEast(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkEast(Plyr, Enemy, State, X, Y, notvalid),
	movesEast(Plyr, Enemy, State, RestMoves, Moves).

movesNE(_, _, _, [], []).
movesNE(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNE(Plyr, Enemy, State, X, Y, valid),
	movesNE(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesNE(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNE(Plyr, Enemy, State, X, Y, notvalid),
	movesNE(Plyr, Enemy, State, RestMoves, Moves).

movesNW(_, _, _, [], []).
movesNW(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNW(Plyr, Enemy, State, X, Y, valid),
	movesNW(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesNW(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkNW(Plyr, Enemy, State, X, Y, notvalid),
	movesNW(Plyr, Enemy, State, RestMoves, Moves).

movesSE(_, _, _, [], []).
movesSE(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSE(Plyr, Enemy, State, X, Y, valid),
	movesSE(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesSE(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSE(Plyr, Enemy, State, X, Y, notvalid),
	movesSE(Plyr, Enemy, State, RestMoves, Moves).

movesSW(_, _, _, [], []).
movesSW(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSW(Plyr, Enemy, State, X, Y, valid),
	movesSW(Plyr, Enemy, State, RestMoves, MovesRest),
	append([X, Y], MovesRest, Moves).

movesSW(Plyr, Enemy, State, [[X, Y]| RestMoves], Moves):-
	checkSW(Plyr, Enemy, State, X, Y, notvalid),
	movesSW(Plyr, Enemy, State, RestMoves, Moves).


checkNorth(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	NewY is Y-1,
	checkNorth(Plyr, Enemy, State, X, NewY, Valid).

checkNorth(_, Enemy, State, X, Y, valid):-
	NewY is Y-1,
	get(State, [X, Y], Enemy),
	get(State, [X, NewY], '.').

checkNorth(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], '.').

checkNorth(Plyr, Enemy, State, X, Y, notvalid):-
	NewY is Y-1,
	get(State, [X, NewY], Enemy),
	get(State, [X, Y], Plyr).

checkSouth(Plyr, Enemy, State, X, Y, Valid):-
	NewY is Y+1,
	get(State, [X, Y], Enemy),
	checkSouth(Plyr, Enemy, State, X, NewY, Valid).

checkSouth(Plyr, Enemy, State, X, Y, Valid):-
	NewY is Y+1,
	get(State, [X, Y], Enemy),
	get(State, [X, Y+1], Plyr),
	checkEndSouth(Plyr, Enemy, State, X, NewY, Valid).

checkSouth(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndSouth(Plyr, _, State, X, Y, valid):-
	NewY is Y-1,
	get(State, [X, Y], Plyr),
	get(State, [X, NewY], .).

checkEndSouth(_, _, _, _, _, notvalid).

checkWest(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkWest(Plyr, Enemy, State, X-1, Y, Valid).

checkWest(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X-1, Y], Plyr),
	checkEndWest(Plyr, Enemy, State, X-1, Y, Valid).

checkWest(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndWest(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X-1, Y], .).

checkEndWest(_, _, _, _, _, notvalid).

checkEast(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkEast(Plyr, Enemy, State, X+1, Y, Valid).

checkEast(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X+1, Y], Plyr),
	checkEndEast(Plyr, Enemy, State, X+1, Y, Valid).

checkEast(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndEast(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X+1, Y], .).

checkEndEast(_, _, _, _, _, notvalid).

checkNW(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkNW(Plyr, Enemy, State, X-1, Y+1, Valid).

checkNW(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X-1, Y+1], Plyr),
	checkEndNW(Plyr, Enemy, State, X-1, Y+1, Valid).

checkNW(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndNW(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X-1, Y+1], .).

checkEndNW(_, _, _, _, _, notvalid).

checkNE(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkNE(Plyr, Enemy, State, X+1, Y+1, Valid).

checkNE(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X+1, Y+1], Plyr),
	checkEndNE(Plyr, Enemy, State, X+1, Y+1, Valid).

checkNE(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndNE(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X+1, Y+1], .).

checkEndNE(_, _, _, _, _, notvalid).

checkSW(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkSW(Plyr, Enemy, State, X-1, Y-1, Valid).

checkSW(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X-1, Y-1], Plyr),
	checkEndSW(Plyr, Enemy, State, X-1, Y-1, Valid).

checkSW(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndSW(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X-1, Y-1], .).

checkEndSW(_, _, _, _, _, notvalid).

checkSE(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	checkSE(Plyr, Enemy, State, X+1, Y-1, Valid).

checkSE(Plyr, Enemy, State, X, Y, Valid):-
	get(State, [X, Y], Enemy),
	get(State, [X+1, Y-1], Plyr),
	checkEndSE(Plyr, Enemy, State, X+1, Y-1, Valid).

checkSE(_, _, State, X, Y, notvalid):-
	get(State, [X, Y], .).

checkEndSE(Plyr, _, State, X, Y, valid):-
	get(State, [X, Y], Plyr),
	get(State, [X+1, Y-1], .).

checkEndSE(_, _, _, _, _, notvalid).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%%
%% define nextState(Plyr,Move,State,NewState,NextPlyr).
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next
%     state) and NextPlayer (i.e. the next player who will move).
%

setEnemiesEast(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X+1,
	setEnemiesEast(Plyr, State, [NewX, Y], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesEast(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesWest(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X-1,
	setEnemiesWest(Plyr, State, [NewX, Y], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesWest(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesNorth(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewY = Y-1,
	setEnemiesNorth(Plyr, State, [X, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesNorth(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesSouth(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= '.',
	NewY = Y+1,
	setEnemiesSouth(Plyr, State, [X, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesSouth(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesSE(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X+1, NewY = Y+1,
	setEnemiesSE(Plyr, State, [NewX, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesSE(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesSW(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X-1, NewY = Y+1,
	setEnemiesSW(Plyr, State, [NewX, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesSW(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesNE(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X+1,
	NewY = Y-1,
	setEnemiesNE(Plyr, State, [NewX, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesNE(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State.

setEnemiesNW(Plyr, State, [X, Y], NewState) :-
	get(State, [X, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	NewX = X-1, NewY = Y-1,
	setEnemiesNW(Plyr, State, [NewX, NewY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).
setEnemiesNW(Plyr, State, [X,Y], NewState) :-
	get(State, [X, Y], Player),
	Player = Plyr,
	NewState = State, !.

makeMoveEast(Plyr, [X, Y], State, NewState) :-
	NextX = X+1,
	get(State, [NextX, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesEast(Plyr, State, [NextX, Y], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).

makeMoveWest(Plyr, [X, Y], State, NewState) :-
	NextX = X-1,
	get(State, [NextX, Y], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesWest(Plyr, State, [NextX, Y], PartState),
	set(PartState, NewState, [X, Y], Plyr).

makeMoveNorth(Plyr, [X, Y], State, NewState) :-
	NextY = Y-1,
	get(State, [X, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesNorth(Plyr, State, [X, NextY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).

makeMoveSouth(Plyr, [X, Y], State, NewState) :-
	NextY = Y+1,
	get(State, [X, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	 setEnemiesSouth(Plyr, State, [X, NextY], PartState), !,
	 set(PartState, NewState, [X, Y], Plyr).


makeMoveSE(Plyr, [X, Y], State, NewState) :-
	NextX = X+1, NextY = Y+1,
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesSE(Plyr, State, [NextX, NextY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).

makeMoveSW(Plyr, [X, Y], State, NewState) :-
	NextX = X-1, NextY = Y+1,
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesSW(Plyr, State, [NextX, NextY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).

makeMoveNE(Plyr, [X, Y], State, NewState) :-
	NextX = X+1, NextY = Y-1,
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesNE(Plyr, State, [NextX, NextY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).

makeMoveNW(Plyr, [X, Y], State, NewState) :-
	NextX = X-1, NextY = Y-1,
	get(State, [NextX, NextY], Enemy),
	Enemy \= Plyr,
	Enemy \= .,
	setEnemiesNW(Plyr, State, [NextX, NextY], PartState), !,
	set(PartState, NewState, [X, Y], Plyr).


nextState(Plyr, Move, State, NewState, NextPlyr):-
	validmove(Plyr, State, Move),
	((Plyr = 2 , NextPlyr = 1) ; (Plyr = 1, NextPlyr = 2)),
	getState(Plyr, Move, State, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	 makeMoveEast(Plyr, Move, State, NewState1),
	 getState(Plyr, Move, NewState1, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveWest(Plyr, Move, State, NewState2),
	getState(Plyr, Move, NewState2, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveNorth(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveSouth(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveSE(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveSW(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveNE(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, Move, State, NewState, NextPlyr) :-
	makeMoveNW(Plyr, Move, State, NewState3),
	getState(Plyr, Move, NewState3, NewState, NextPlyr).
getState(Plyr, _, State, State, NextPlyr) :-
	((Plyr = 2 , NextPlyr = 1) ; (Plyr = 1, NextPlyr = 2)).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%%
%% define validmove(Plyr,State,Proposed).
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, Proposed):-
	moves(Plyr, State, MoveLst),
	member(Proposed, MoveLst).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define h(State,Val).
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, -40):-
	winner(State, 1), !.

h(State, 40) :-
	winner(State, 2), !.

h(State, 0) :-
	tie(State), !.

h(State, Val) :-
	score(State, 1, Player1Score),
	score(State, 2, Player2Score),
	Val is Player2Score - Player1Score.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define lowerBound(B).
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(-100).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define upperBound(B).
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.


upperBound(100).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value).
%. . . . . .
%. . . . . .
%. . 1 2 . .
%. . 2 1 . .
%. . . . . .
%. . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'],
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...],
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2
%Yes
%?-
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2).
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 2 1 . .
% . . . . . .
% . . . . . .
%
% . . . . . .
% . . . . . .
% . . 1 2 . .
% . . 1 1 . .
% . . 1 . . .
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.',
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.',
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :-
	nth0( Y, Board, ListY),
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value).

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0,
    Y1 is Y-1,
    set( RestRows, NewRestRows, [X, Y1], Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value).

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :-
	Index > 0,
	Index1 is Index-1,
	setInList( RestList, NewRestList, Index1, Value).

