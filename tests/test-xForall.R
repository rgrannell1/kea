
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xForall")

	forall(
		"xForall of truth is true`",
		list(coll = test_cases$collection),
		xForall(Truth, coll, coll),
		given =
			length(coll) > 0
	)

	forall(
		"xForall of falsity is false",
		list(coll = test_cases$collection),
		!xForall(Falsity, coll, coll),
		given =
			length(coll) > 0
	)

	forall(
		"xForall of moot is false",
		list(coll = test_cases$collection),
		!xForall(Moot, coll, coll),
		given =
			length(coll) > 0
	)

	# might occasionally fail!

	forall(
		"xForall of a function that sometimes yields true is false",
		list(ints = G$integers()),
		!xForall(
			function (a, b) {
				a %% 2 == 0 || b %% 2 == 1
			}, ints, ints),
		given =
			length(ints) > 0
	)

message("arrow $ xForall")

	forall(
		"fn $ xForall",
		list(coll = test_cases$collection),
		x_(Truth)$xForall(coll, coll)$x(),
		given =
			length(coll) > 0
	)

	forall(
		"coll $ xForall",
		list(coll = test_cases$collection),
		x_(coll)$xForall(Truth)$x(),
		given =
			length(coll) > 0
	)
