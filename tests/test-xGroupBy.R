
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

	forall(
		"grouping an length one is as list(coll)",
		test_cases$num_positive_integer,
		xGroupBy(xI, num) %is% list(list(num, list( num )))
	)

kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xGroupBy")

	over(coll) +

	describe("grouping an empty collection is list()") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xGroupBy(identity, coll) %is% list()
	) +

	run()
