
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xChop")

	forall(
		"chopping into one slice is sortof identity",
		test_cases$collection,
		xChop(1, coll) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)

	forall(
		"chopping into Inf is as.list",
		test_cases$collection,
		xChop(Inf, coll) %equals% as.list(coll),
		given =
			length(coll) > 0
	)
