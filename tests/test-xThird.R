
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xThird')

forall("third always returns the third element of a collection",
	test_cases$collection,
	expect =
		xThird(coll) %equals% coll[[3]],
	given =
		length(coll) >= 3
)

message('arrow $ xThird')

	forall("third always returns the third element of a collection",
		test_cases$collection,
		x_(coll)$xThird()$x() %equals% coll[[3]],
		given =
			length(coll) >= 3
	)

message('arrow $ xThird...')

	forall("third always returns the third element of a collection",
		test_cases$collection,
		x_(coll)$xThird...(coll)$x() %equals% coll,
		given =
			length(coll) >= 3
	)


message('arrow $ x_Third')

	forall("third always returns the third element of a collection",
		test_cases$collection,
		x_(coll)$x_Third() %equals% coll[[3]],
		given =
			length(coll) >= 3
	)

message('arrow $ x_Third...')

	forall("third always returns the third element of a collection",
		test_cases$collection,
		x_(coll)$x_Third...(coll) %equals% coll,
		given =
			length(coll) >= 3
	)
