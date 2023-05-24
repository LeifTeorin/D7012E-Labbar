% /* --------------------------------------------------------------- */
%
%          D O   N O T   C H A N G E   T H I S   F I L E.
%
% /* --------------------------------------------------------------- */

% Generic interactive Game shell using Minimax search

% Copyright (c) 2002 Craig Boutilier
%
% modified for SWI by Fahiem Bacchus
%
% modified 2018 by Håkan Jonsson to have the human player
% modified 2020 by Joakim Nilsson to give moves in tuple (not list) format
% automatically make random moves

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This script works like play.pl for the computer player. The
% difference compared with play.pl is that the human player, below
% nicknamed "Stupid", now plays automatically by making random
% moves: It tries, at random, to place a stone at empty positions
% and without any AI.
%
% This script can be used to carry out automated test-runs of your AI code
% in othello.pl as a complement to manual playing/testing.
%
% NB! The name of each procedure that has been added ends with XYZ to
% lower (eliminate?) the risk of name collisions with procedures
% in othello.pl.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% stonesXYZ(State, Plyr, Stones) - counts number of stones Plyr has on board State

stonesXYZ([],_,0).
stonesXYZ([F|Rest],P,N) :-
    stonesXYZ(Rest,P,N2),
    stonesXXYZ(F,P,N3),
    N is N2+N3.
stonesXXYZ([],_,0).
stonesXXYZ([X|T],P,N) :-
    stonesXXYZ(T,P,N2),
    (X == P
        -> N is N2+1
        ;  N is N2
    ).

otherXYZ(1,2).
otherXYZ(2,1).

currentScoreXYZ(State) :-
    stonesXYZ(State,1,N1),
    stonesXYZ(State,2,N2),
    write('Stupid (player 1) has '),
    write(N1),
    write(' pieces while Computer (player 2) has '),
    writeln(N2),
    nl.

play :- initialize(InitState,Plyr), playgame(Plyr,InitState),!.

playgame(_,State) :-
    winner(State,Winner), !,
    nl,
    writeln('*** G A M E   O V E R ***'),
    (Winner == 1
        -> writeln('Stupid (player 1) wins!')
        ;  writeln('Computer (player 2) wins.')
    ),
    writeln('*************************'),
    showState(State),
    currentScoreXYZ(State).

playgame(_,State) :-
    tie(State), !,
    nl,writeln('Game ended with no winner!'),
    writeln('Final board:'),
    showState(State),
    currentScoreXYZ(State).


playgame(Plyr,State) :-
  getmove(Plyr,State,Move),
  write('Result: The move chosen is : '),
  writeln(Move),
  nextState(Plyr,Move,State,NewState,NextPlyr),
  playgame(NextPlyr,NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getmove(Player,State,Move)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get move for player 1 (stupid)

freeXYZ(State,[X,Y]) :-
    member(X,[0,1,2,3,4,5]),
    member(Y,[0,1,2,3,4,5]),
    get(State,[X,Y],Value),
    Value == '.'. % only return empty squares

getmove(1,State,Move) :-
    showState(State),
    currentScoreXYZ(State),
    setof(Mv, freeXYZ(State,Mv),Moves), % collect all empty squares
    random_permutation(Moves, Perm), % permute the squares
    % writeln(Perm), % uncomment to see all empty squares
    member(Proposed,Perm), % try one square at a time
    Proposed = [X, Y],
    ProposedNewFormat = [X, Y],
    write('Stupid is trying move '),write(ProposedNewFormat),
    (validmove(1,State,ProposedNewFormat)
        -> !, Move = ProposedNewFormat, nl
        ;  writeln(' ... *** Invalid! ***'), fail % backtrack and try another square
    ).

getmove(1,_,n) :-
    writeln('Stupid is forced to play a null move').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get move for player 2 (computer)
% - do this using minimax evaluation
%
% SET DEPTH BOUND HERE
%  Depth should be set appropriately below.

getmove(2,State,Move) :-
    showState(State),
    currentScoreXYZ(State),
    writeln('Computer is moving...'),
    MaxDepth is 4,
    mmeval(2,State,_,Move,MaxDepth,SeF),
    write('Computer move computed by searching '),
    write(SeF),
    write(' states with max depth '),
    writeln(MaxDepth).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mini-max eval
% mmeval(Plyr,State,Value,BestMove,Depth,StatesSearched)
%     - does minimax evaluation
%  of State, assuming move by Plyr (1 = max, 2 = min) to bound Depth.
%  returns Value of the state, as well a BestMove for the player (either
%  the move with max or min value depending on player)
%  Assume evaluation function h


% if State is terminal, use evaluation function
mmeval(_,State,Val,_,_,1) :- terminal(State), !,
  %writeln('Evaluation reached Terminal'),
  h(State,Val).

% if depth bound reached, use evaluation function
mmeval(_,State,Val,_,0,1) :-  !,
  %writeln('Evaluation reached Depth Bnd'),
  h(State,Val).

% FOR MAX PLAYER
% we assume that if player has no moves available, the position is
% terminal and would have been caught above

mmeval(1,St,Val,BestMv,D,SeF) :-
  moves(1,St,MvList), !,
% length(MvList,L),
% write('Evaluating '), write(L), write(' moves at Plyr 1 depth '), writeln(D),
  lowerBound(B), % a value strictly less than worst value max can get
  evalMoves(1,St,MvList,B,null,Val,BestMv,D,0,SeI), % Best so far set to lowerbnd
  SeF is SeI + 1.  %searched the current state as well as

% FOR MIN PLAYER
% we assume that if player has no moves available, the position is
% terminal and would have been caught above

mmeval(2,St,Val,BestMv,D,SeF) :-
  moves(2,St,MvList), !,
% length(MvList,L),
% write('Evaluating '), write(L), write(' moves for Plyr 2 at depth '), writeln(D),
  upperBound(B), % a value strictly less than worst value max can get
  evalMoves(2,St,MvList,B,null,Val,BestMv,D,0,SeI), % Best so far set to upperbnd
  SeF is SeI + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalMoves(Plyr,State,MvList,ValSoFar,MvSoFar,Val,BestMv,D,Se,SeF)
%
% - evaluates all moves in MvList for Plyr at State.
% - returns minimax value Val of State by recursively evaluating each
%   successor state, returning BestMv (move that guarantees this value)
% - it has as arguments, the best ValSoFar and best MvSoFar of any other
%   moves that have already been processed (i.e., that have been
%   removed from the current list of moves).
% - a depth bound D is enforced.
%  Se is number of states searched so far.
%  SeF is the total number of states searched to evalute all of these moves.


% if no moves left, return best Val and Mv so far (and number of
% states searched.
evalMoves(_,_,[],Val,BestMv,Val,BestMv,_,Se,Se) :- !.
%	write('No more moves Val = '), write(Val),
%	write(' BestMv = '), write(BestMv), nl.

% otherwise evaluate current move (by calling mmeval on the player/state
% that results from this move), and replace current Best move and value
% by this Mv/Value if value is "better"

evalMoves(1,St,[Mv|Rest],ValSoFar,MvSoFar,Val,BestMv,D,Se,SeF) :-
  nextState(1,Mv,St,NewSt,NextPlyr), !,
%  write('evalMoves 1: '), write(Mv), write(' D='), write(D), write(' S='), write(Se), showState(NewSt),
  Dnew is D - 1,
  mmeval(NextPlyr,NewSt,MvVal,_,Dnew,SeI), !,
  maxMove(ValSoFar,MvSoFar,MvVal,Mv,NewValSoFar,NewMvSoFar),
  SeNew is Se + SeI,
  evalMoves(1,St,Rest,NewValSoFar,NewMvSoFar,Val,BestMv,D,SeNew,SeF).


evalMoves(2,St,[Mv|Rest],ValSoFar,MvSoFar,Val,BestMv,D,Se,SeF) :-
  nextState(2,Mv,St,NewSt,NextPlyr), !,
%  write('evalMoves 2: '), write(Mv), write(' D='), write(D), write(' S='), write(Se), showState(NewSt),
  Dnew is D - 1,
  mmeval(NextPlyr,NewSt,MvVal,_,Dnew,SeI),  !,
  minMove(ValSoFar,MvSoFar,MvVal,Mv,NewValSoFar,NewMvSoFar),
  SeNew is Se + SeI,
  evalMoves(2,St,Rest,NewValSoFar,NewMvSoFar,Val,BestMv,D,SeNew,SeF).

%% Return the max of best so far and the current move.
maxMove(V1,M1,V2,_,V1,M1) :- V1 >= V2.
maxMove(V1,_,V2,M2,V2,M2) :- V1 < V2.
%% Return the min of best so far and the current move.
minMove(V1,M1,V2,_,V1,M1) :- V1 =< V2.
minMove(V1,_,V2,M2,V2,M2) :- V1 > V2.
