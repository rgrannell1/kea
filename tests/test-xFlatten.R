
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xFlatten")

	forall(
		"flattening an empty collection returns the empty list",
		test_cases$positive_with_recursive_zero,
		xFlatten(num, coll) %is% list(),
		given =
			num > 0
	)

	forall(
		"flattening to 1 is unlist",
		test_cases$positive_with_collection,
		xFlatten(num, coll) %is% as.list(unlist(coll))
	)

	forall(
		"flattening a vector is as.list",
		test_cases$letters_and_index,
		xFlatten(num, as.character(coll)) %is% as.list(coll)
	)

	forall(
		"flattening to Inf is the identity",
		test_cases$positive_with_recursive_zero,
		xFlatten(Inf, coll) %is% as.list(coll)
	)
