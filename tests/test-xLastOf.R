
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xLastOf')

	forall(
		"first always returns the last element of a collection",
		test_cases$collection,
		xLastOf(coll) %equals% coll[[ length(coll) ]],
		given =
			length(coll) > 0
	)

