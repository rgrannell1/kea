
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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
message("arrow $ xForall")

	forall(
		"function $ xForall",
		test_cases$collection,
		x_(Truth)$xForall(list(coll, coll))$x_(),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ xForall",
		test_cases$collection,
		x_(coll)$xForall(Truth)$x_(),
		given =
			length(coll) > 0
	)

message("arrow $ x_Forall")

	forall(
		"function $ x_Forall",
		test_cases$collection,
		x_(Truth)$x_Forall(list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ x_Forall",
		test_cases$collection,
		x_(coll)$x_Forall(Truth),
		given =
			length(coll) > 0
	)
