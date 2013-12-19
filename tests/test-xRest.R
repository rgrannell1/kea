
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xRest")

	forall(
		"xRest of an empty collection always yields the empty list.",
		test_cases$collection,
		xRest(coll) %equals% list(),
		given =
			length(coll) == 0
	)

	forall(
		"xRest of a list shortens the list by one (usually)",
		test_cases$collection,
		length(xRest(coll)) == length(coll) - 1,
		given =
			length(coll) > 0
	)

message("arrow $ xRest")

	forall(
		"xRest of a list shortens the list by one (usually)",
		test_cases$collection,
		length(x_(coll)$xRest()$x()) == length(coll) - 1,
		given =
			length(coll) > 0
	)

message("arrow $ x_Rest")

	forall(
		"x_Rest of a list shortens the list by one (usually)",
		test_cases$collection,
		length(x_(coll)$x_Rest()) == length(coll) - 1,
		given =
			length(coll) > 0
	)
