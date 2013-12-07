
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

	# might occasionally fail!

	forall(
		"xForall of a function that sometimes yields true is false",
		test_cases$integers,
		!xForall(
			function (a, b) {
				a %% 2 == 0 || b %% 2 == 1
			},
			list(coll, coll)
		),
		given =
			length(ints) > 0
	)

message("arrow $ xForall")

	forall(
		"fn $ xForall",
		test_cases$collection,
		x_(Truth)$xForall(list(coll, coll))$x(),
		given =
			length(coll) > 0
	)

	forall(
		"coll $ xForall",
		test_cases$collection,
		x_(coll)$xForall(Truth)$x(),
		given =
			length(coll) > 0
	)
