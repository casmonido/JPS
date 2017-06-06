start_A_star( InitState, PathCost, StepLimit, MaxSteps, MaxNodesCheckedNum) :-

	MaxSteps > StepLimit,

	score(InitState, 0, 0, InitCost, InitScore),

	StepCounter is 0,

	search_A_star( [node(InitState, nil, nil, InitCost , InitScore )], [ ], StepCounter, StepLimit, PathCost, MaxNodesCheckedNum).


start_A_star( InitState, PathCost, StepLimit, MaxSteps, MaxNodesCheckedNum) :-

	MaxSteps > StepLimit,

	NewStepLimit is StepLimit + 1,

	start_A_star( InitState, PathCost, NewStepLimit, MaxSteps, MaxNodesCheckedNum).
	




search_A_star(Queue, ClosedSet, StepCounter, StepLimit, PathCost, MaxNodesCheckedNum) :-

	fetch(Node, Queue, ClosedSet, RestQueue, MaxNodesCheckedNum),

	continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, PathCost, MaxNodesCheckedNum).
	






continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, _, _, path_cost(Path, Cost), _) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, Path, MaxNodesCheckedNum)  :-

	NewStepCounter is StepCounter + 1,

	is_possible_to_go_forward(StepLimit, StepCounter),

	write('Krok '), write(StepCounter), write('\nStan kolejki:\n'),

	write_n_nodes(MaxNodesCheckedNum, RestQueue),

	write('Wybrany wezel: \n'), 

	write_element(Node),

	write(' - rozwijac ten wezel czy przejsc do kolejnego (jesli istnieje)? - t./n.:\n'),

	read(t),

	expand(Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [ Node | ClosedSet ], NewStepCounter, StepLimit, Path, MaxNodesCheckedNum).






is_possible_to_go_forward(StepLimit, StepCounter) :-

	StepLimit >= StepCounter, !.


is_possible_to_go_forward(_, _) :-

	write('Powrot do fetch\n\n'), 

	fail.





write_row(_, _, [], _) :-

	write('\n').


write_row(RowNum, ColNum, [pos(X, ColNum/RowNum) | _], WholeList) :-

	ColNum < 3, !,

	write(X), write(' '),

	NewColNum is ColNum + 1,

	write_row(RowNum, NewColNum, WholeList, WholeList).



write_row(RowNum, ColNum, [pos(X, ColNum/RowNum) | Rest], WholeList) :-

	write(X),

	write('\n') , !.


write_row(RowNum, ColNum, [ _ | Rest], WholeList) :-

	write_row(RowNum, ColNum, Rest, WholeList).
	



write_element(node(State, Action, Parent, Cost, Score)) :-
	write('\t'), 
	write_row(3, 1, State, State),
	write('\t'), 
	write_row(2, 1, State, State),
	write('\t'), 
	write_row(1, 1, State, State),
	write('\tFscore: '), write(Score), write('\n').




write_n_nodes(Num, []).

write_n_nodes(Num, [Node | RestQueue]) :- 

	NewNum is Num - 1,

	Num > 0,

	write_element(Node),

	write_n_nodes(NewNum, RestQueue).





fetch(Node, [ FirstNode |RestQueue], ClosedSet, NewRest, NNodes) :-

	member(FirstNode , ClosedSet), !,

	fetch(Node, RestQueue, ClosedSet , NewRest, NNodes).


fetch(node(State, Action,Parent, Cost, Score),
			[node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet,  RestQueue, NNodes) :- 
	
	NNodes > 0.


fetch(Node,	[FirstNode |RestQueue], ClosedSet, [FirstNode | NewRest], NNodes) :-

	NNodes > 0,

	NewNNodes is NNodes - 1,

	fetch(Node, RestQueue, ClosedSet, NewRest, NewNNodes).






expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-

	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore) ) , NewNodes) .





score(State, ParentCost, StepCost, Cost, FScore)  :-

	Cost is ParentCost + StepCost ,

	hScore(State, HScore),

	FScore is Cost + HScore .






insert_new_nodes( [ ], Queue, Queue) .


insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-

	insert_p_queue(Node, Queue, Queue1),

	insert_new_nodes( RestNodes, Queue1, NewQueue) .





insert_p_queue(Node,  [ ], [Node] )  :- ! .


insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-

	FScore >= FScore1,  ! ,

	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .


insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]) .






build_path(node(nil, _, _, _, _ ), _, Path, Path) :- ! .


build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path)  :-

	del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1) ,

	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1,
						[Action/EndState|PartialPath],Path) .





del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).





goal([ 		pos(0 , 3/1), pos(1 , 1/3), pos(2 , 2/3), 
			pos(3 , 3/3), pos(4 , 1/2), pos(5 , 2/2), 
			pos(6 , 3/2), pos(7 , 1/1), pos(8 , 2/1) 	]) .




succ( [ pos(0, EmptyPos) | TilePositions], _, 1, [pos(0, NewEmptyPos) | NewTilePositions ] ) :-

	find_neighbour(EmptyPos, TilePositions, NewEmptyPos, NewTilePositions) .


find_neighbour(	EmptyPos, [pos(Neighb, NeighbPos)|RestPositions],
				NeighbPos, [pos(Neighb, EmptyPos)|RestPositions]) :-

		adjacent(EmptyPos, NeighbPos).


find_neighbour(EmptyPos, [T |RestPositions], NewEmptyPos, [T | NewPositions]) :-
			
		find_neighbour(EmptyPos, RestPositions, NewEmptyPos, NewPositions) .





adjacent(X1/Y1, X2/Y1) :-

		DiffX is X1-X2,

		abs(DiffX, 1).


adjacent(X1/Y1, X1/Y2) :-
		
		DiffY is Y1-Y2,

		abs(DiffY, 1).




abs(X, X) :-
		
	X >=0, ! .


abs(X, AbsX) :-

	AbsX is -X.



hScore( [ _ | Positions ], HScore) :-

	goal( [ _ | GoalPositions ]),

	sum_of_distances(Positions, GoalPositions, HScore).





sum_of_distances( [ ], [ ], 0 ) .


sum_of_distances( [pos( _, Pos) | RestPositions], [pos( _, GoalPos) | RestGoalPositions], Sum) :-

		distance( Pos, GoalPos, Dist ),

		sum_of_distances(RestPositions, RestGoalPositions, Sum1),

		Sum is Sum1 + Dist .




distance( X1 / Y1, X2 / Y2, Dist) :-

	DiffX is X1 - X2,

	abs(DiffX, DistX),

	DiffY is Y1 - Y2,

	abs(DiffY, DistY),

	Dist is DistX + DistY .