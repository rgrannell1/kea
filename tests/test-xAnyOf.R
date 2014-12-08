
kea ::: load_test_dependencies(environment())

unit_test("xAnyOf")

	over(coll) +

	it('xAnyOf with identity is !any.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,

		xAnyOf(identity, coll) %is% (length(which(coll)) > 0)
	) +

	it('partially applying with true is false') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAnyOf(function (x) True, coll) == True
	) +

	it('partially applying with false is true') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAnyOf(function (x) False, coll) == False,
		xAnyOf(function (x) Na,    coll) == False
	) +

	run()
