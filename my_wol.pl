%%%%%%%%%%%%%%    my_wol.pl    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
count(_, [], 0) :- !. 

count(X, [X|T], N) :- 
	count(X, T, N2), 
	N is N2 + 1.    

count(X, [Y|T], N) :- 
	X \= Y,    
	count(X, T, N).

smaller(X, Y, X) :-
	(X =< Y).

smaller(X, Y, Y) :-
	(Y < X).

smallest([Head|[]], Head).

smallest([Head|Tail], Answer) :-
	smallest(Tail, What),
	smaller(What, Head, Answer).

larger(X, Y, X) :-
	(X=\=250, X >= Y);
    Y=:=250.

larger(X, Y, Y) :-
    (Y=\=250, Y > X);
    X=:=250.

largest([Head|[]], Head).

largest([Head|Tail], Answer) :-
	largest(Tail, What),
	larger(What, Head, Answer).

list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
	Item_sum is Item1 + Item2,
	list_sum([Item_sum|Tail], Total).

average(List, Average):- 
	list_sum(List, Sum),
	length(List, Length),
	Length > 0, 
	Average is Sum/Length.


% test strategy with accumlator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% base case accumulator, print out results
acc_test_strategy(0, _, _, Win_list, Num_moves_list, Times_list):-
	count('b', Win_list, B_wins),
	count('r', Win_list, R_wins),
	count('exhaust', Win_list, Exhausts), 
	count('draw', Win_list, Drawn),
	count('stalemate', Win_list, Stalemates),
	Draws is Exhausts + Drawn + Stalemates,
	smallest(Num_moves_list, Shortest),
	largest(Num_moves_list, Largest),
	average(Num_moves_list, Average),
	average(Times_list, Average_time),
	write('Blue wins: '), write(B_wins), nl,
	write('Red wins: '), write(R_wins), nl,
	write('Draws: '), write(Draws), nl,
	write('Shortest: '), write(Shortest), write(' moves'), nl,
	write('Longest: '), write(Largest), write(' moves'), nl,
	write('Average: '), write(Average), write(' moves'), nl,
	write('Average time: '), write(Average_time), write(' ms'), nl.

% play game and then append winner to winner list which is parsed in base case
acc_test_strategy(N, P1_strategy, P2_strategy, Win_list, Num_moves_list, Times_list):-
	N=\=0,
	statistics(walltime, [_ | [_]]),
	play(quiet, P1_strategy, P2_strategy, NumMoves, WinningPlayer),
	statistics(walltime, [_ | [ExecutionTime]]),
	append([WinningPlayer], Win_list, New_win_list),
	append([NumMoves], Num_moves_list, New_num_moves_list),
	append([ExecutionTime], Times_list, New_times_list),
	N1 is N - 1,
	acc_test_strategy(N1, P1_strategy, P2_strategy, New_win_list, New_num_moves_list, New_times_list).

test_strategy(N, P1_strategy, P2_strategy) :-
	acc_test_strategy(N, P1_strategy, P2_strategy, [], [], []).

% helpers for strategies %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possible_moves('b', [Alive,OtherPlayerAlive], PossMoves):-
    findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossMoves).

possible_moves('r', [OtherPlayerAlive,Alive], PossMoves):-
    findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossMoves).

my_number_pieces('b', [Blue, _], Quantity):-
        length(Blue, Quantity).

my_number_pieces('r', [_, Red], Quantity):-
        length(Red, Quantity).

opp_number_pieces('b', [_, Red], Quantity):-
        length(Red, Quantity).

opp_number_pieces('r', [Blue, _], Quantity):-
        length(Blue, Quantity).

my_alter_board('b', [A,B,MA,MB], [Alives,Reds], [NewAlives,Reds]):-
    alter_board([A,B,MA,MB], Alives, NewAlives).

my_alter_board('r', [A,B,MA,MB], [Blues,Alives], [Blues,NewAlives]):-
    alter_board([A,B,MA,MB], Alives, NewAlives).

% bloodlust %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%cycle through all possible moves doing a conway crank, then count the number of opponents pieces
%if count is lowest so far, store move and number in accumulators
%return the stored move
%return newboard after move but before crank

acc_bloodlust(_, _, BestBoard, BestMove, BestBoard, BestMove, [], _).

%if current move being tested returns lower opponent piece count
acc_bloodlust(PieceColour, Board, NewBoard, Move, _, _, [CurrentMove|Tail], OppPieceCount):-
	my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    opp_number_pieces(PieceColour, CrankedBoard, PotentialCount),
    PotentialCount =< OppPieceCount,
    acc_bloodlust(PieceColour, Board, NewBoard, Move, PotentialBoard, CurrentMove, Tail, PotentialCount).

%otherwise
acc_bloodlust(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, [CurrentMove|Tail], OppPieceCount):-
    my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    opp_number_pieces(PieceColour, CrankedBoard, PotentialCount),
    PotentialCount > OppPieceCount,
    acc_bloodlust(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, Tail, OppPieceCount).

    
bloodlust(PieceColour, Board, NewBoard, Move):-
    possible_moves(PieceColour, Board, [CurrentMove|Tail]), [CurrentMove|Tail] \= [],
    my_alter_board(PieceColour, CurrentMove, Board, BestBoard),
    next_generation(BestBoard, CrankedBoard),
    opp_number_pieces(PieceColour, CrankedBoard, OppPieceCount),
    acc_bloodlust(PieceColour, Board, NewBoard, Move, BestBoard, CurrentMove, [CurrentMove|Tail], OppPieceCount).
    
%test bloodlust:
% bloodlust('b', [[[1,4],[2,3],[2,4],[3,2]],[[1,3]]], N, M).
% move should be [3,2,2,2]

% self preservation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%self preservation is like bloodlust but instead maximise own pieces
acc_self_pres(_, _, BestBoard, BestMove, BestBoard, BestMove, [], _).

%if current move being tested returns greater self piece count
acc_self_pres(PieceColour, Board, NewBoard, Move, _, _, [CurrentMove|Tail], MyPieceCount):-
	my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, PotentialCount),
    PotentialCount >= MyPieceCount,
    acc_self_pres(PieceColour, Board, NewBoard, Move, PotentialBoard, CurrentMove, Tail, PotentialCount).

%otherwise
acc_self_pres(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, [CurrentMove|Tail], MyPieceCount):-
    my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, PotentialCount),
    PotentialCount < MyPieceCount,
    acc_self_pres(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, Tail, MyPieceCount).


self_preservation(PieceColour, Board, NewBoard, Move):-
	possible_moves(PieceColour, Board, [CurrentMove|Tail]), [CurrentMove|Tail] \= [],
    my_alter_board(PieceColour, CurrentMove, Board, BestBoard),
    next_generation(BestBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, MyPieceCount),
    acc_self_pres(PieceColour, Board, NewBoard, Move, BestBoard, CurrentMove, [CurrentMove|Tail], MyPieceCount).

%test self_preservation:
% self_preservation('r', [[[1,2],[2,1],[3,2],[3,3],[3,4]],[[2,3]]], N, M).
% move should not be [2,3,2,2]

% land grab %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%land_grab is max PieceDiff = Number of Player\92s pieces \96 Number of Opponent\92s pieces
acc_land_grab(_, _, BestBoard, BestMove, BestBoard, BestMove, [], _).

%if current move being tested returns greater piece diff
acc_land_grab(PieceColour, Board, NewBoard, Move, _, _, [CurrentMove|Tail], PieceDiff):-
	my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, PotentialMe),
    opp_number_pieces(PieceColour, CrankedBoard, PotentialOpp),
    PotentialDiff is PotentialMe - PotentialOpp,
    PotentialDiff >= PieceDiff,
    acc_land_grab(PieceColour, Board, NewBoard, Move, PotentialBoard, CurrentMove, Tail, PotentialDiff).

%otherwise
acc_land_grab(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, [CurrentMove|Tail], PieceDiff):-
    my_alter_board(PieceColour, CurrentMove, Board, PotentialBoard),
    next_generation(PotentialBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, PotentialMe),
    opp_number_pieces(PieceColour, CrankedBoard, PotentialOpp),
    PotentialDiff is PotentialMe - PotentialOpp,
    PotentialDiff < PieceDiff,
    acc_land_grab(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, Tail, PieceDiff).


land_grab(PieceColour, Board, NewBoard, Move):-
	possible_moves(PieceColour, Board, [CurrentMove|Tail]), [CurrentMove|Tail] \= [],
    my_alter_board(PieceColour, CurrentMove, Board, BestBoard),
    next_generation(BestBoard, CrankedBoard),
    my_number_pieces(PieceColour, CrankedBoard, PotentialMe),
    opp_number_pieces(PieceColour, CrankedBoard, PotentialOpp),
    PieceDiff is PotentialMe - PotentialOpp,
    acc_land_grab(PieceColour, Board, NewBoard, Move, BestBoard, CurrentMove, [CurrentMove|Tail], PieceDiff).

other_colour('b', 'r').
other_colour('r', 'b').

% minimax %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%minimax - we cycle through possible moves and choose the best result considering opponent does landgrab after that move
acc_minimax(_, _, BestBoard, BestMove, BestBoard, BestMove, [], _).

%if current move being tested returns greater piece diff after opponent does landgrab move
acc_minimax(PieceColour, Board, NewBoard, Move, _, _, [CurrentMove|Tail], PieceDiff):-
	my_alter_board(PieceColour, CurrentMove, Board, NextBoard1),
    next_generation(NextBoard1, CrankedBoard1),
    other_colour(PieceColour, OtherColour),
    land_grab(OtherColour, CrankedBoard1, NextBoard2, _),
    next_generation(NextBoard2, FinalBoard),
    my_number_pieces(PieceColour, FinalBoard, PotentialMe),
    opp_number_pieces(PieceColour, FinalBoard, PotentialOpp),
    PotentialDiff is PotentialMe - PotentialOpp,
    PotentialDiff >= PieceDiff,
    acc_minimax(PieceColour, Board, NewBoard, Move, NextBoard1, CurrentMove, Tail, PotentialDiff).

%otherwise
acc_minimax(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, [CurrentMove|Tail], PieceDiff):-
	my_alter_board(PieceColour, CurrentMove, Board, NextBoard1),
    next_generation(NextBoard1, CrankedBoard1),
    other_colour(PieceColour, OtherColour),
    land_grab(OtherColour, CrankedBoard1, NextBoard2, _),
    next_generation(NextBoard2, FinalBoard),
    my_number_pieces(PieceColour, FinalBoard, PotentialMe),
    opp_number_pieces(PieceColour, FinalBoard, PotentialOpp),
    PotentialDiff is PotentialMe - PotentialOpp,
    PotentialDiff < PieceDiff,
    acc_minimax(PieceColour, Board, NewBoard, Move, BestBoard, BestMove, Tail, PieceDiff).

minimax(PieceColour, Board, NewBoard, Move):-
    possible_moves(PieceColour, Board, [CurrentMove|Tail]), [CurrentMove|Tail] \= [],
    my_alter_board(PieceColour, CurrentMove, Board, NextBoard1),
    next_generation(NextBoard1, CrankedBoard1),
    other_colour(PieceColour, OtherColour),
    land_grab(OtherColour, CrankedBoard1, NextBoard2, _),
    next_generation(NextBoard2, FinalBoard),
    my_number_pieces(PieceColour, FinalBoard, PotentialMe),
    opp_number_pieces(PieceColour, FinalBoard, PotentialOpp),
    PieceDiff is PotentialMe - PotentialOpp,
    acc_minimax(PieceColour, Board, NewBoard, Move, NextBoard1, CurrentMove, [CurrentMove|Tail], PieceDiff).

