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

	StepLimit >= StepCounter,

	expand(Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [ Node | ClosedSet ], NewStepCounter, StepLimit, Path).





member_node(node(Element, _, _, _, _), [node(Element, Action, Parent, Cost, Score) | RestClosedSet], node(Element, Action, Parent, Cost, Score) ).


member_node(Node, [ _ | RestClosedSet], OtherNode ) :-
	
	member_node(Node, RestClosedSet, OtherNode).





fetch(node(Element, Action, Parent, Cost, Score), [ node(Element, Action, Parent, Cost, Score) |RestQueue], ClosedSet, NewRest, NNodes) :-

	member_node(node(Element, Action, Parent, Cost, Score) , ClosedSet, node(Element, Action1, Parent1, Cost1, Score1)),

	Cost1 > Cost, !.


fetch(Node, [ node(Element, Action, Parent, Cost, Score) |RestQueue], ClosedSet, NewRest, NNodes) :-

	member_node(node(Element, Action, Parent, Cost, Score) , ClosedSet, node(Element, Action1, Parent1, Cost1, Score1)), !,

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






hScore(a, 0).
hScore(b, 0).
hScore(c, 4).
hScore(d, 0).
hScore(e, 4).
hScore(f, 3).

goal(f).

succ(a, Action, 5, b).
succ(a, Action, 2, c).

succ(b, Action, 5, d).

succ(c, Action, 5, e).

succ(d, Action, 0, f).

succ(e, Action, 2, d).
succ(e, Action, 1, f).
