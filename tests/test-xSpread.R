
kea ::: load_test_dependencies(environment())

message("xUnspread")

	over(coll) +

	describe('all functions become unary') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		do.call( xSpread(identity), as.list(coll) ) %is% as.list(coll)
	) +

	run()

