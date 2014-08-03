
kea ::: load_test_dependencies(environment())

message("xUnspread")

	over(coll) +

	describe('all functions become unary') +
	holdsWhen(
		is_collection(coll) && length(coll) > 0,

		do.call( xSpread(identity), as.list(coll) ) %is% as.list(coll)
	) +

	run()

