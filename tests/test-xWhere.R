
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xWhere (+)")

	over(coll) +

	describe("length one corner cases") +
	when(
		is_collection(coll) && length(coll) == 0,
		xWhere(coll) %is% integer(0)
	) +

	describe("length one corner cases") +
	when(
		TRUE,
		xWhere(TRUE) == 1,
		xWhere(FALSE) %is% integer(0),
		xWhere(NA) %is% integer(0)
	) +

	run()

message("xWhere (-)")

	over(coll) +

	describe("length one corner cases") +
	failsWhen(
		!is.logical(coll) && length(coll) > 0,
		xWhere(coll)
	) +

	run()
