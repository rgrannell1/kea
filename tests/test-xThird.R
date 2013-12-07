
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xThird')

forall("third always returns the third element of a collection",
	list(coll = G$collection),
	expect =
		xThird(coll) %equals% coll[[3]],
	given =
		length(coll) <= 3
)

message('arrow $ xThird')

	forall("third always returns the third element of a collection",
		list(coll = G$collection),
		x_(coll)$xThird() %equals% coll[[3]],
		given =
			length(coll) <= 3
	)
