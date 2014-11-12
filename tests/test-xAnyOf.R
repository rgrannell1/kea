
kea ::: load_test_dependencies()

message("xAnyOf")

	over(coll) +

	describe('xAnyOf with identity is !any.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,

		xAnyOf(identity, coll) %is% (length(which(coll)) > 0)
	) +

	describe('partially applying with true is false') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAnyOf(function (x) True, coll) == True
	) +

	describe('partially applying with false is true') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAnyOf(function (x) False, coll) == False,
		xAnyOf(function (x) Na,    coll) == False
	) +

	run()
