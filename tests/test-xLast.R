
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xLast')

	forall(
		"first always returns the last element of a collection",
		test_cases$collection,
		xLast(coll) %equals% coll[[length(coll)]],
		given =
			length(coll) > 0
	)

message('arrow $ xLast')

	forall(
		"collection $ xLast",
		test_cases$collection,
		x_(coll)$xLast()$x() %equals% coll[[length(coll)]],
		given =
			length(coll) > 0
	)
