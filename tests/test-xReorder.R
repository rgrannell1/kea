
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xReorder')

	forall(
		"empty list is identity",
		list(),
		xReorder(list(), list()) %equals% list()
	)

	forall(
		"permuting with seq along is identity",
		test_cases$collection,
		xReorder(seq_along(coll), list(coll)) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)
