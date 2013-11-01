
message('xFourth')

forall("fourth always returns the fourth element of a collection",
	list(coll = G$collection),
	expect =
		xFourth(coll) %equals% coll[[4]],
	given =
		length(coll) <= 4
)
