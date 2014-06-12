
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('normal functions can be partially applied (+)')

	over(val, coll) +

	describe('unary functions are partially appliable') +
	when(
		is_collection(coll),
		xIdentity(val) %equals% val,
		xIdentity(val) %equals% xIdentity()(val)
	) +

	describe('binary functions are partially appliable') +
	when(
		is_collection(coll),
		xMap(identity, coll) %equals% as.list(coll),
		xMap(identity, coll) %equals% xMap(identity)(coll),
		xMap(identity, coll) %equals% xMap()()(identity)(coll)
	) +

	describe('trinary functions are partially appliable') +
	when(
		is_collection(coll),
		xFold('+', 0L, seq_along(coll)) %equals% sum(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+')(0L, seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+')(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+', 0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+')(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+')()(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %equals% xFold('+')(0L)()(seq_along(coll))
	) +

	run()

message('variadic functions can be partially applied (+)')

	over(val, coll) +

	describe('unary variadic functions are partially appliable') +
	when(
		is_collection(coll),
		xJoin_(coll) %equals% as.list(coll),
		xJoin_(coll) %equals% xJoin_()(coll)
	) +

	describe('binary variadic functions work') +
	when(
		TRUE,
		xIs(val, val),
		xIs(val)(val),
		xIs(val)()(val),
		xIs()(val)(val)
	) +

	run()

message('normal methods can be partially applied (+)')

	over(val, coll) +

	describe('binary methods are partially appliable') +
	when(
		is_collection(coll),
		x_(coll) $ x_Map(identity) %equals% x_(coll) $ x_Map()(identity)
	) +

	describe('trinary methods are partially appliable') +
	when(
		is_collection(coll),
		x_(coll) $ x_Fold(c, c()) %equals% x_(coll) $ x_Fold(c)(c()),
		x_(c) $ x_Fold(c(), coll) %equals% x_(c) $ x_Fold(c())(coll)
	) +

	run()
