
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xLastOf')

	forall(
		"first always returns the last element of a collection",
		test_cases$collection,
		xLastOf(coll) %equals% coll[[ length(coll) ]],
		given =
			length(coll) > 0
	)

