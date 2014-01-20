
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSplitAt')

#	forall(
#		"splitting an empty collection yields the empty list",
#		test_cases$nonnegative_with_collection_zero,
#		xSplitAt(num, coll) %equals% list()
#	)

#	forall(
#		"splitting with all indices is as.list^2",
#		test_cases$collection,
#		xSplitAt(seq_along(coll), coll) %equals% Map(as.list, as.list(coll)),
#		given =
#			length(coll) > 0
#	)

#	forall(
#		"splitting with 0 yields an empty list and the list",
#		test_cases$collection,
#		xSplitAt(0, coll) %equals% list(list(), as.list(coll)),
#		given =
#			length(coll) > 0
#	)

#	forall(
#		"splitting with a large number yields the list and an empty list",
#		test_cases$collection,
#		xSplitAt(length(coll) + 1, coll) %equals% list(as.list(coll), list()),
#		given =
#			length(coll) > 0
#	)

