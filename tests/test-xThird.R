
message('xThird')

forall("third always returns the third element of a collection",
	list(coll = G$collection),
	expect =
		xThird(coll) %equals% coll[[3]],
	given =
		length(coll) <= 3
)
