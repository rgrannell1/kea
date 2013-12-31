
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSecond')

forall("second always returns the second element of a collection",
	test_cases$collection,
	expect =
		xSecond(coll) %equals% coll[[2]],
	given =
		length(coll) >= 2
)

message('arrow $ xSecond')

	forall("second always returns the second element of a collection",
		test_cases$collection,
		x_(coll)$xSecond()$x_() %equals% coll[[2]],
		given =
			length(coll) >= 2
	)

message('arrow $ xSecond...')

message('arrow $ x_Second')

	forall("second always returns the second element of a collection",
		test_cases$collection,
		x_(coll)$x_Second() %equals% coll[[2]],
		given =
			length(coll) >= 2
	)

message('arrow $ x_Second...')
