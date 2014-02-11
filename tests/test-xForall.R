
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xForall")

	forall(
		"xForall of truth is true`",
		test_cases$collection,
		xForall(Truth, list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"xForall of falsity is false",
		test_cases$collection,
		!xForall(Falsity, list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"xForall of moot is false",
		test_cases$collection,
		!xForall(Moot, list(coll, coll)),
		given =
			length(coll) > 0
	)