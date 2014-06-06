
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xChop")

	forall(
		"chopping into one slice is sortof identity",
		test_cases$collection,
		xChop(1, coll) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)


kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xChop (+)")

	over(coll) +

	describe("xChopping infinite times / length times is as.list") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChop(Inf,          coll) %equals% lapply(coll, list),
		xChop(length(coll), coll) %equals% lapply(coll, list)
	) +

	describe("xChop once is almost identity") +
	when(
		is_collection(coll) && length(coll) > 0,
		xChop(1, coll) %equals% list(as.list(coll))
	) +

	run()
