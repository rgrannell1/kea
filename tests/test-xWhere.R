
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xWhere (+)")

	over(coll) +

	describe("length one corner cases") +
	when(
		is_collection(coll) && length(coll) == 0,
		xWhere(coll) %equals% integer(0)
	) +

	describe("length one corner cases") +
	when(
		TRUE,
		xWhere(TRUE) == 1,
		xWhere(FALSE) %equals% integer(0),
		xWhere(NA) %equals% integer(0)
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
