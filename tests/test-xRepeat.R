
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xRepeat")

	forall(
		"repeating the empty list yields the empty list.",
		test_cases$positive_with_collection_zero,
		xRepeat(num, coll) %equals% list()
	)

	forall(
		"repeating a collection is done by end-to-end concatenation.",
		test_cases$positive_with_collection,
		xRepeat(num, coll) %equals% as.list(rep(coll, num))
	)

message("arrow $ xRepeat")

	forall(
		"repeating the empty list yields the empty list.",
		test_cases$positive_with_collection_zero,
		x_(coll)$xRepeat(num)$x_() %equals% list()
	)

message("arrow $ x_Repeat")

	forall(
		"repeating the empty list yields the empty list.",
		test_cases$positive_with_collection_zero,
		x_(coll)$x_Repeat(num) %equals% list()
	)