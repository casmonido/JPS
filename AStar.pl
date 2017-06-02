start_A_star( InitState, PathCost, StepLimit, MaxSteps, MaxNodesCheckedNum) :-

	MaxSteps > StepLimit,

	score(InitState, 0, 0, InitCost, InitScore),

	StepCounter is 0,

	search_A_star( [node(InitState, nil, nil, InitCost , InitScore )], [ ], StepCounter, StepLimit, PathCost, MaxNodesCheckedNum).


start_A_star( InitState, PathCost, StepLimit, MaxSteps, MaxNodesChecked) :-

	MaxSteps > StepLimit,

	NewStepLimit is StepLimit + 1,

	start_A_star( InitState, PathCost, NewStepLimit, MaxSteps, MaxNodesChecked).
	




search_A_star(Queue, ClosedSet, StepCounter, StepLimit, PathCost, MaxNodesCheckedNum) :-

	fetch(Node, Queue, ClosedSet , RestQueue, MaxNodesCheckedNum),

	continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, PathCost, MaxNodesCheckedNum).
	




continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, _, _, path_cost(Path, Cost), MaxNodesCheckedNum) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, Path, MaxNodesCheckedNum)  :-

	NewStepCounter is StepCounter + 1,

	StepLimit >= StepCounter,

	expand(Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [ Node | ClosedSet ], NewStepCounter, StepLimit, Path, MaxNodesCheckedNum).





change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, Parent, Cost, FScore) | NextSet], AlreadyDone, ResultSet) :-

	\+ member(El, AlreadyDone), !,

	write('Change child value '), write(Parent), write(Cost), write(El), write('\n'),

	NewCost is Cost - CostDiff,

	NewScore is FScore - CostDiff,

	add_to_end(node(El, Action, Parent, NewCost, NewScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, NextSet, [El | AlreadyDone], ResultSetOne),

	change_children_values(El, CostDiff, [], ResultSetOne, [El | AlreadyDone], ResultSet).


change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, Parent, Cost, FScore) | NextSet], AlreadyDone, ResultSet) :-

	write('Change child value '), write(Parent), write(Cost), write(El), write('\n'),

	NewCost is Cost - CostDiff,

	NewScore is FScore - CostDiff,

	add_to_end(node(El, Action, Parent, NewCost, NewScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, NextSet, AlreadyDone, ResultSet).


change_children_values(_, _, PrevSet, [], _, PrevSet).


change_children_values(Parent, CostDiff, PrevSet, [X | NextSet], AlreadyDone, ResultSet)  :-

	add_to_end(X, PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, NextSet, AlreadyDone, ResultSet).





fetch(Node, [ FirstNode |RestQueue], ClosedSet, NewRest, NNodes) :-

	in_closed_set_with_worse_score_if_so_swap(FirstNode, ClosedSet, NewClosedSet, CostDiff, Parent),

	write('Przeszczep galezi '), write(Parent), write('\n'),

	change_children_values(Parent, CostDiff, [], NewClosedSet, [], UpdatedClosedSet),

	change_children_values(Parent, CostDiff, [], RestQueue, [], NewQueue),

	fetch(Node, NewQueue, UpdatedClosedSet, NewRest, NNodes).



fetch(Node, [ FirstNode |RestQueue], ClosedSet, NewRest, NNodes) :-

	member(FirstNode, ClosedSet), !,

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

	Cost is ParentCost + StepCost,

	hScore(State, HScore),

	FScore is Cost + HScore.






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





in_closed_set_with_worse_score_if_so_swap(node(Element, A1, Parent, Cost, Score), 
	[node(Element, A2, OldParent, OldCost, OldScore) | RestClosedSet], 
			[node(Element, A1, Parent, Cost, Score) | RestClosedSet], CostDiff, Element) :-

	OldCost > Cost, 

	CostDiff is OldCost - Cost.


in_closed_set_with_worse_score_if_so_swap(Node, [ _ | RestClosedSet], 
	NewClosedSet, CostDiff, ParentEl) :-
	
	in_closed_set_with_worse_score_if_so_swap(Node, RestClosedSet, NewClosedSet, CostDiff, ParentEl).





add_to_end(Elem, [First | RestSet], [First | ReturnSet]) :-

			add_to_end(Elem, RestSet, ReturnSet).


add_to_end(Elem, [], [Elem]).








hScore(a, 3).
hScore(b, 8).
hScore(c, 1).
hScore(d, 1).
hScore(e, 0).
hScore(f, 27).

goal(f).

succ(a, _, 1, b).
succ(a, _, 5, c).
succ(a, _, 1, d).

succ(b, _, 1, c).
succ(b, _, 5, f).

succ(c, _, 1, e).
succ(c, _, 1, f).

succ(d, _, 7, e).
succ(d, _, 8, f).

succ(e, _, 2, f).