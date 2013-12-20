
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xSegment")

	forall(
		"one divides into lists of one.",
		test_cases$collection,
		length(xSegment(1, coll)) == length(coll),
		given =
			length(coll) > 0
	)

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		xSegment(Inf, coll) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)


message("arrow $ xSegment")

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		x_(coll)$xSegment(Inf)$x() %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)

message("arrow $ x_Segment")

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		x_(coll)$x_Segment(Inf) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)
