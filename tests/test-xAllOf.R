
kea ::: load_test_dependencies(environment())

unit_test("xAllOf")

	over(coll) +

	it('xAllOf with identity is !any.') +
	holdsWhen(
		is.logical(coll) && length(coll) > 0,
		xAllOf(identity, coll) %is% length(which(!coll)) == 0
	) +

	it('partially applying with true is false') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAllOf(function (x) True, coll) == True
	) +

	it('partially applying with false is true') +
	holdsWhen(
		suchThat $ not_empty_collection(coll),

		xAllOf(function (x) False, coll) == False,
		xAllOf(function (x) Na,    coll) == False
	) +

	run()

