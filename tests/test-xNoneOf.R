
kea ::: load_test_dependencies(environment())

message("xNoneOf")

	over(coll) +

	describe('xNoneOf with identity is !any.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,

		xNoneOf(identity, coll) %is% length(which(coll)) == 0
	) +

	describe('partially applying with true is false') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xNoneOf(function (x) True, coll) == False
	) +

	describe('partially applying with false is true') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xNoneOf(function (x) False, coll) == True,
		xNoneOf(function (x) Na,    coll) == True
	) +

	run()

