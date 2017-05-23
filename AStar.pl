start_A_star( InitState, PathCost, StepLimit ) :-

	score(InitState, 0, 0, InitCost, InitScore),

	StepCounter is 0,

	search_A_star( [node(InitState, nil, nil, InitCost , InitScore )], [ ], StepCounter, StepLimit, PathCost) .


start_A_star( InitState, PathCost, StepLimit ) :-

	5 > StepLimit,

	NewStepLimit is StepLimit + 1,

	start_A_star( InitState, PathCost, NewStepLimit ).
	




search_A_star(Queue, ClosedSet, StepCounter, StepLimit, PathCost) :-

	fetch(Node, Queue, ClosedSet , RestQueue, 1),

	continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, PathCost).
	




continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, _, _, path_cost(Path, Cost) ) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, Path)  :-

	NewStepCounter is StepCounter + 1,

	is_possible_to_go_forward(StepLimit, StepCounter),

	write('Krok '), write(StepCounter), write(': [ '),

	write_n_nodes(2, RestQueue),

	write('], Wybrany wezel: '), write_node(Node),

	write('czy kontynuowac? - t./n.:\n'),

	read('t'),

	expand(Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [ Node | ClosedSet ], NewStepCounter, StepLimit, Path).



is_possible_to_go_forward(StepLimit, StepCounter) :-

	StepLimit >= StepCounter, !.


is_possible_to_go_forward(StepLimit, StepCounter) :-

	write('Powrot do fetch\n\n'), 

	fail.



write_node(node(State, Action, Parent, Cost, Score)) :-

	write(State), write('/'), write(Score), write(' ').


write_n_nodes(Num, []).

write_n_nodes(Num, [Node | RestQueue]) :- 

	NewNum is Num - 1,

	Num > 0,

	write_node(Node),

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






hScore(State, HScore) :-
	HScore is 0.

goal(e).

succ(a, Action, 0, b).
succ(a, Action, 0, c).
succ(a, Action, 0, d).
succ(b, Action, 0, d).
succ(d, Action, 0, e).