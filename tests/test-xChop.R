
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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

message("arrow $ xChop")

	forall(
		"chopping into Inf is as.list",
		test_cases$collection,
		x_(coll)$xChop(Inf)$x_() %equals% as.list(coll),
		given =
			length(coll) > 0
	)

message("arrow $ x_Chop")

	forall(
		"chopping into Inf is as.list",
		test_cases$collection,
		x_(coll)$x_Chop(Inf) %equals% as.list(coll),
		given =
			length(coll) > 0
	)
