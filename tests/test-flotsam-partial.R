
kea ::: load_test_dependencies(environment())

message('normal functions can be partially applied')

	over(val, coll) +

	describe('unary functions are partially appliable') +
	holdsWhen(
		is_collection(coll),

		xIdentity(val) %is% val,
		xIdentity(val) %is% xIdentity()(val)
	) +

	describe('binary functions are partially appliable') +
	holdsWhen(
		is_collection(coll),

		xMap(identity, coll) %is% as.list(coll),
		xMap(identity, coll) %is% xMap(identity)(coll),
		xMap(identity, coll) %is% xMap()()(identity)(coll),


		xMap(identity, coll) %is% as.list(coll),
		xMap(identity, coll) %is% xMap(coll = coll)(identity),
		xMap(identity, coll) %is% xMap()()(coll = coll)(identity)
	) +

	describe('trinary functions are partially appliable') +
	holdsWhen(
		is_collection(coll),

		xFold('+', 0L, seq_along(coll)) %is% sum(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+')(0L, seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+')(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+', 0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+')(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+')()(0L)(seq_along(coll)),
		xFold('+', 0L, seq_along(coll)) %is% xFold('+')(0L)()(seq_along(coll))
	) +

	run()






message('variadic functions can be partially applied')

	over(val, coll) +

	describe('unary variadic functions are partially appliable') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,
		xJoin_(coll) %is% as.list(coll),
		xJoin_(coll) %is% xJoin_()(coll)
	) +

	describe('binary variadic functions work') +
	holdsWhen(
		TRUE,
		xIs(val, val),
		xIs(val)(val),
		xIs(val)()(val),
		xIs()(val)(val)
	) +

	run()
