
message('xLast')

forall("first always returns the last element of a collection",
	list(coll = G$collection),
	expect =
		xLast(coll) %equals% coll[[length(coll)]],
	given =
		length(coll) <= 1
)
