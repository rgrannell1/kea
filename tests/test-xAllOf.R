
kea ::: load_test_dependencies(environment())

message("xAllOf")

	over(coll) +

	describe('xAllOf with identity is !any.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,
		xAllOf(identity, coll) %is% length(which(!coll)) == 0
	) +

	describe('partially applying with true is false') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAllOf(function (x) True, coll) == True
	) +

	describe('partially applying with false is true') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAllOf(function (x) False, coll) == False,
		xAllOf(function (x) Na,    coll) == False
	) +

	run()

