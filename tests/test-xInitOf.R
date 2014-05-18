
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xInitOf")

	forall(
		"init of an empty collection always yields the empty list.",
		test_cases$collection,
		expect =
			xInitOf(coll) %equals% list(),
		given =
			length(coll) == 0
	)

	forall(
		"init of a list shortens the list by one",
		test_cases$collection,
		expect =
			length(xInitOf(coll)) == length(coll) - 1,
		given =
			length(coll) > 0
	)

