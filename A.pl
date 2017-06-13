start_A_star( InitState, PathCost, StepLimit, MaxSteps, NumNodesToCheck ) :-

	MaxSteps >= StepLimit,

	score(InitState, 0, 0, InitCost, InitScore),

	StepCounter is 0,

	search_A_star( [node(InitState, nil, nil, InitCost , InitScore )], [ ], StepCounter, StepLimit, PathCost, NumNodesToCheck).


start_A_star( InitState, PathCost, StepLimit, MaxSteps, NumNodesToCheck) :-

	MaxSteps > StepLimit,

	NewStepLimit is StepLimit + 1,

	start_A_star( InitState, PathCost, NewStepLimit, MaxSteps, NumNodesToCheck).
	




search_A_star(Queue, ClosedSet, StepCounter, StepLimit, PathCost, NumNodesToCheck) :-

	fetch(Node, Queue, ClosedSet, RestQueue, NumNodesToCheck),

	continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, PathCost, NumNodesToCheck).
	




continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, _, _, path_cost(Path, Cost), _) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, Path, NumNodesToCheck)  :-

	NewStepCounter is StepCounter + 1,

	StepLimit >= StepCounter,

	write('Krok '), write(StepCounter),

	write('\n\tStan (reszty) kolejki:\n'), write_n_nodes(NumNodesToCheck, RestQueue),

	write('\n\tWybrany wezel: \n'), write_element(Node), write('\n\n'), !,

	expand(Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [ Node | ClosedSet ], NewStepCounter, StepLimit, Path, NumNodesToCheck).




write_n_nodes(_, []).

write_n_nodes(0, _).

write_n_nodes(Num, [Node | RestQueue]) :- 

	NewNum is Num - 1,

	Num > 0,

	write_element(Node),

	write_n_nodes(NewNum, RestQueue).



write_element(node(State, Action, Parent, Cost, Score)) :-

	write('\t'), write(State), write('/'), write(Score), write(' ').



fetch(Node, [ FirstNode |RestQueue], ClosedSet, NewRest, NumNodesToCheck) :-

	member(FirstNode , ClosedSet), !,

	fetch(Node, RestQueue, ClosedSet , NewRest, NumNodesToCheck).


fetch(node(State, Action,Parent, Cost, Score),
			[node(State, Action,Parent, Cost, Score) |RestQueue], _,  RestQueue, NumNodesToCheck) :- 
	
	NumNodesToCheck > 0.


fetch(Node,	[FirstNode |RestQueue], ClosedSet, [FirstNode | NewRest], NumNodesToCheck) :-

	NumNodesToCheck > 0,

	NewNumNodesToCheck is NumNodesToCheck - 1,

	fetch(Node, RestQueue, ClosedSet, NewRest, NewNumNodesToCheck).






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




hScore(a, 3).
hScore(b, 8).
hScore(c, 1).
hScore(d, 1).
hScore(e, 0).
hScore(f, 27).
hScore(g, 100).

goal(g).

succ(a, ab, 1, b).
succ(a, ac, 1, c).
succ(a, ad, 1, d).

succ(b, bc, 1, c).
succ(b, bf, 5, f).

succ(c, ce, 1, e).
succ(c, cf, 1, f).

succ(d, de, 7, e).
succ(d, df, 8, f).

succ(e, ef, 2, f).

succ(f, fg, 2, g).