
kea ::: load_test_dependencies(environment())

message("xWhere")

	over(coll) +

	describe("length one corner cases") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && !is_named(coll),

		xWhere(coll) %is% integer(0)
	) +

	describe("length one corner cases") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 && is_named(coll),

		xWhere(coll) %is% as_named(integer(0))
	) +

	describe("length one corner cases") +
	holdsWhen(
		TRUE,

		xWhere(TRUE) == 1,
		xWhere(FALSE) %is% integer(0),
		xWhere(NA)    %is% integer(0)
	) +

	describe("names are preserved for true elements.") +
	holdsWhen(
		is_logical(coll),

		names(xWhere(coll)) %is% names(Filter(function (x) identical(x, True), coll))
	) +

	run()
