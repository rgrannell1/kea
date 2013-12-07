
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFirst')

forall("first always returns the first element of a collection",
	test_cases$collection,
	expect =
		xFirst(coll) %equals% coll[[1]],
	given =
		length(coll) <= 1
)

message('arrow $ xFirst')

	forall("first always returns the first element of a collection",
		test_cases$collection,
		x_(coll)$xFirst() %equals% coll[[1]],
		given =
			length(coll) <= 1
	)
