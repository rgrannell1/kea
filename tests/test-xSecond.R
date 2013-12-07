
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSecond')

forall("second always returns the second element of a collection",
	test_cases$collection,
	xSecond(coll) %equals% coll[[2]],
	given =
		length(coll) >= 2
)

message('arrow $ xSecond')

	forall("second always returns the second element of a collection",
		test_cases$collection,
		x_(coll)$xSecond()$x() %equals% coll[[2]],
		given =
			length(coll) >= 2
	)
