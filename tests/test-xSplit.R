
message('xSplit')

forall(
	"splitting an empty collection yields the empty list",
	list(ind = G$nonnegative(), coll = G$collection_zero),
	xSplit(ind, coll) %equals% list()
)

forall(
	"splitting with 0 yields an empty list and the list",
	list(coll = G$collection()),
	xSplit(0, coll) %equals% list(list(), as.list(coll)),
	given =
		length(coll) > 0
)

forall(
	"splitting with a large number yields the list and an empty list",
	list(coll = G$collection()),
	xSplit(length(coll) + 1, coll) %equals% list(as.list(coll), list()),
	given =
		length(coll) > 0
)
