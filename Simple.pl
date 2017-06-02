
func(Parent, Val, PrevSet, [node(El, Value, Parent) | NextSet], AlreadyDone, ReturnSet)  :-

	\+ member(El, AlreadyDone), !,

	Value2 is Value - Val,

	add_to_end(node(El, Value2, Parent), PrevSet, WholePrev),

	func(Parent, Val, WholePrev, NextSet, [El | AlreadyDone], ReturnSetOne),

	func(El, Val, [], ReturnSetOne, [El | AlreadyDone], ReturnSet).


func(Parent, Val, PrevSet, [node(El, Value, Parent) | NextSet], AlreadyDone, ReturnSet)  :-

	Value2 is Value - Val,

	add_to_end(node(El, Value2, Parent), PrevSet, WholePrev),

	func(Parent, Val, WholePrev, NextSet, AlreadyDone, ReturnSet).


func(Parent, Val, PrevSet, [], _, PrevSet).


func(Parent, Val, PrevSet, [X | NextSet], AlreadyDone, ReturnSet)  :-

	add_to_end(X, PrevSet, WholePrev),

	func(Parent, Val, WholePrev, NextSet, AlreadyDone, ReturnSet).



add_to_end(Elem, [First | Set], [First |Return]) :-

	add_to_end(Elem, Set, Return).

add_to_end(Elem, [], [Elem]).


start(ReturnSet) :-

	func(a, 10, [], [node(b, 11, a), node(g, 12, b), node(c, 11, a), node(c, 11, a), node(a, 10, x), node(i, 12, c), node(d, 11, a),

		node(e, 12, b), node(f, 12, b), 
		node(h, 12, c), node(j, 13, g)], [], ReturnSet).