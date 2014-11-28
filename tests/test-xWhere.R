
kea ::: load_test_dependencies(environment())

unit_test("xWhere")

	over(coll) +

	it("returns integer(0) for empty input") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xWhere(coll) %is% keep_names(coll, integer(0))
	) +

	it("returns 1 for a single true value") +
	holdsWhen(
		True,

		xWhere(TRUE) == 1
	) +

	it("returns integer(0) for a single true value") +
	holdsWhen(
		True,

		xWhere(FALSE) %is% integer(0),
		xWhere(NA)    %is% integer(0)
	) +


	it("preserves true names.") +
	holdsWhen(
		suchThat $ is_logical(coll),

		names(xWhere(coll)) %is% names(Filter(function (x) identical(x, True), coll))
	) +

	run()
