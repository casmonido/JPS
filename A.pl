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

	write('Krok '), write(StepCounter),

	write('\n\tStan kolejki:\n'), write_n_nodes(100, Queue),

	write('\n\tStan zbioru zamknietych stanow:\n'), write_n_nodes(100, ClosedSet),

	fetch(Node, Queue, ClosedSet, UpdatedClosedSet, RestQueue, NumNodesToCheck),

	write('\n\tWybrany wezel: \n'), write_element(Node), write('\n\n'), 

	read(t), !,

	continue(Node, RestQueue, UpdatedClosedSet, StepCounter, StepLimit, PathCost, NumNodesToCheck).
	




continue(node(State, Action, Parent, Cost, _ ) , _  ,  ClosedSet, _, _, path_cost(Path, Cost), _) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, RestQueue, ClosedSet, StepCounter, StepLimit, Path, NumNodesToCheck)  :-

	NewStepCounter is StepCounter + 1,

	StepLimit >= StepCounter,

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

	write('\t'), write(State), write('/'), write(Cost), write('/'), write(Score), write('/'), write(Parent), write(' ').






fetch(ReturnNode, [FirstNode | RestQueue], ClosedSet, NewUpdatedClosedSet, NewUpdatedRestQueue, NumNodesToCheck) :-

	in_closed_set_with_worse_score_if_so_swap(FirstNode, ClosedSet, NewClosedSet, CostDiff, Parent), !,

	write('\nPrzeszczep galezi '), write(Parent), write('\n'),

	change_children_values(Parent, CostDiff, [], RestQueue, [], NewRestQueue, [], []),

	change_children_values(Parent, CostDiff, [], NewClosedSet, [], UpdatedClosedSet, NewRestQueue, NewRestQueue1),

	fetch(ReturnNode, NewRestQueue1, UpdatedClosedSet, NewUpdatedClosedSet, NewUpdatedRestQueue, NumNodesToCheck).



fetch(Node, [ node(FirstNode, _,_,_,_) |RestQueue], ClosedSet, UpdatedClosedSet, NewRest, NumNodesToCheck) :-

	member(node(FirstNode, _,_,_,_) , ClosedSet), !,

	fetch(Node, RestQueue, ClosedSet, UpdatedClosedSet, NewRest, NumNodesToCheck).



fetch(node(State, Action,Parent, Cost, Score),
			[node(State, Action,Parent, Cost, Score) |RestQueue], ClosedSet, ClosedSet, RestQueue, NumNodesToCheck) :- 
	
	NumNodesToCheck > 0.



fetch(Node,	[FirstNode |RestQueue], ClosedSet, UpdatedClosedSet, [FirstNode | NewRest], NumNodesToCheck) :-

	NumNodesToCheck > 0,

	NewNumNodesToCheck is NumNodesToCheck - 1,

	fetch(Node, RestQueue, ClosedSet, UpdatedClosedSet, NewRest, NewNumNodesToCheck).







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
hScore(b, 50).
hScore(c, 3).
hScore(d, 50).
hScore(e, 2).
hScore(f, 100).
hScore(g, 200).

goal(g).

succ(a, ab, 1, b).
succ(a, ac, 1, c).
succ(a, ad, 1, d).

succ(b, bc, -1, c).
succ(b, bf, 1, f).

succ(c, ce, 1, e).
succ(c, cf, 3, f).

succ(d, de, 0, e).
succ(d, df, 1, f).

succ(e, ef, 1, f).

succ(f, fg, 1, g).







change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, Parent, Cost, FScore) | RestNextSet], 
	AlreadyDone, ResultSet, [], []) :-

	\+ member(El, AlreadyDone), !,

	write(Parent), write('. Modifying node '), write(El), write(', who will be parent\n'),

	NewCost is Cost - CostDiff,

	NewScore is FScore - CostDiff,

	add_to_end(node(El, Action, Parent, NewCost, NewScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, RestNextSet, [El | AlreadyDone], ResultSetOne, [], []),

	change_children_values(El, CostDiff, [], ResultSetOne, [El | AlreadyDone], ResultSet, [], []), !.



change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, Parent, Cost, FScore) | RestNextSet], 
	AlreadyDone, ResultSet, OtherQueue, NewOtherQueue) :-

	\+ member(El, AlreadyDone), !,

	write(Parent), write('. Modifying node '), write(El), write(', which will be next parent\n'),

	NewCost is Cost - CostDiff,

	NewScore is FScore - CostDiff,

	add_to_end(node(El, Action, Parent, NewCost, NewScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, RestNextSet, [El | AlreadyDone], ResultSetOne, OtherQueue, NewOtherQueue1),

	change_children_values(El, CostDiff, [], ResultSetOne, [El | AlreadyDone], ResultSet, NewOtherQueue1, NewOtherQueue2),

	change_children_values(El, CostDiff, [], NewOtherQueue2, [El | AlreadyDone], NewOtherQueue, [], NewOtherQueue), !.




change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, Parent, Cost, FScore) | NextSet], 
	AlreadyDone, ResultSet, OtherQueue, NewOtherQueue) :-

	write(Parent), write('. Modifying node '), write(El), write('\n'),

	NewCost is Cost - CostDiff,

	NewScore is FScore - CostDiff,

	add_to_end(node(El, Action, Parent, NewCost, NewScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, NextSet, AlreadyDone, ResultSet, OtherQueue, NewOtherQueue), !.


change_children_values(Parent, _, PrevSet, [], _, PrevSet, OtherQueue, OtherQueue) :-

	write(Parent), write('. NextSet empty \n'), !.


change_children_values(Parent, CostDiff, PrevSet, [node(El, Action, X, Cost, FScore) | NextSet], 
	AlreadyDone, ResultSet, OtherQueue, NewOtherQueue)  :- 

	write(Parent), write('. Skipping '), write(El), write('\n'),

	add_to_end(node(El, Action, X, Cost, FScore), PrevSet, WholePrev),

	change_children_values(Parent, CostDiff, WholePrev, NextSet, AlreadyDone, ResultSet, OtherQueue, NewOtherQueue), !.













in_closed_set_with_worse_score_if_so_swap(node(Element, Action, Parent, Cost, Score), 
										[node(Element, _, _, OldCost, _) | RestClosedSet], 
			[node(Element, Action, Parent, Cost, Score) | RestClosedSet], CostDiff, Element) :-

	OldCost > Cost, !, 

	CostDiff is OldCost - Cost.



in_closed_set_with_worse_score_if_so_swap(Node, [ OtherNode | RestClosedSet], 
										[OtherNode | NewRestClosedSet], CostDiff, NodeName) :-
	
	in_closed_set_with_worse_score_if_so_swap(Node, RestClosedSet, NewRestClosedSet, CostDiff, NodeName).





add_to_end(Elem, [First | RestSet], [First | ReturnSet]) :-

			add_to_end(Elem, RestSet, ReturnSet).


add_to_end(Elem, [], [Elem]).
