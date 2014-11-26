
kea ::: load_test_dependencies(environment())

message("xWhere")

	over(coll) +

	it("length one corner cases") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ not_named)(coll),

		xWhere(coll) %is% integer(0)
	) +

	it("length one corner cases") +
	holdsWhen(
		and_(suchThat $ is_empty_collection, suchThat $ is_named)(coll),

		xWhere(coll) %is% as_named(integer(0))
	) +

	it("length one corner cases") +
	holdsWhen(
		True,

		xWhere(TRUE) == 1,
		xWhere(FALSE) %is% integer(0),
		xWhere(NA)    %is% integer(0)
	) +

	it("names are preserved for true elements.") +
	holdsWhen(
		suchThat $ is_logical(coll),

		names(xWhere(coll)) %is% names(Filter(function (x) identical(x, True), coll))
	) +

	run()
