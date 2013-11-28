
message('xDrop')

forall(
	"the empty collection always yields the empty list.",
	list(num = G$nonnegative(), coll = G$collection_zero),
	xDrop(num, coll) %equals% list()
)

forall(
	"dropping yields the correct collection.",
	list(num = G$positive(), coll = G$collection()),
	{
		ind <- min(length(coll), num)
		xDrop(num, coll) %equals% as.list(coll[1:ind])
	},
	given =
		length(coll) > 0
)
