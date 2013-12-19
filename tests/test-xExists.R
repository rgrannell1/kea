
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xExists")

	forall(
		"xExists of truth is true`",
		test_cases$collection,
		xExists(Truth, list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"xExists of falsity is false",
		test_cases$collection,
		!xExists(Falsity, list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"xExists of moot is false",
		test_cases$collection,
		!xExists(Moot, list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"xExists of a function that sometimes yields true is true",
		test_cases$integers,
		xExists(
			function (a, b) {
				a %% 2 == 0 || b %% 2 == 1
			}, list(coll, coll)),
		given =
			length(coll) > 0
	)

message("arrow $ xExists")

	forall(
		"function $ xExists",
		test_cases$collection,
		x_(Truth)$xExists(list(coll, coll))$x(),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ xExists",
		test_cases$collection,
		x_(coll)$xExists(Truth)$x(),
		given =
			length(coll) > 0
	)

message("arrow $ x_Exists")

	forall(
		"function $ x_Exists",
		test_cases$collection,
		x_(Truth)$x_Exists(list(coll, coll)),
		given =
			length(coll) > 0
	)

	forall(
		"collection $ x_Exists",
		test_cases$collection,
		x_(coll)$x_Exists(Truth),
		given =
			length(coll) > 0
	)