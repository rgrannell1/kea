
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xGroup")

	forall(
		"one divides into lists of one.",
		test_cases$collection,
		length(xGroup(1, coll)) == length(coll),
		given =
			length(coll) > 0
	)

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		xGroup(Inf, coll) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)


message("arrow $ xGroup")

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		x_(coll)$xGroup(Inf)$x_() %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)

message("arrow $ x_Group")

	forall(
		"infinite doesn't divide collection.",
		test_cases$collection,
		x_(coll)$x_Group(Inf) %equals% list(as.list(coll)),
		given =
			length(coll) > 0
	)
