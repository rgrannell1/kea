
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFourth')

forall("fourth always returns the fourth element of a collection",
	test_cases$collection,
	expect =
		xFourth(coll) %equals% coll[[4]],
	given =
		length(coll) >= 4
)

message('arrow $ xFourth')

	forall("fourth always returns the fourth element of a collection",
		test_cases$collection,
		x_(coll)$xFourth()$x() %equals% coll[[4]],
		given =
			length(coll) >= 4
	)

message('arrow $ xFourth...')

	forall("fourth always returns the fourth element of a collection",
		test_cases$collection,
		do.call(x_(coll)$xFourth..., as.list(coll)) %equals% coll[[4]],
		given =
			length(coll) >= 4
	)

message('arrow $ x_Fourth')

	forall("fourth always returns the fourth element of a collection",
		test_cases$collection,
		x_(coll)$x_Fourth() %equals% coll[[4]],
		given =
			length(coll) >= 4
	)

message('arrow $ x_Fourth...')

	forall("fourth always returns the fourth element of a collection",
		test_cases$collection,
		x_(coll)$x_Fourth...(coll) %equals% coll,
		given =
			length(coll) >= 4
	)
