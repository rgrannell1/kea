
context("xMap: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xMap(fn, coll) %equals% list()
)

forall(
	"mapping identity over the list preserves its contents & length.",
	list(coll = G$collection),
	{
		xMap(identity, coll) %equals% as.list(coll) &&
		length(xMap(identity, coll)) == length(coll)
	}
)

forall(
	"mapping increment increments the list",
	G$standard$inc_over_ints(),
	{
		all( unlist(xMap(fn, coll)) == unlist(coll) + 1 )
	}
)

context("arrow $ xMap: positive controls")

forall(
	"collection.xMap selects even-numbers.",
	G$standard$inc_over_ints(),
	{
		all( unlist(x_(coll)$xMap(fn)$x()) == unlist(coll) + 1 )
	}
)

forall(
	"function.xMap selects even-numbers.",
	G$standard$inc_over_ints(),
	{
		all( unlist(x_(fn)$xMap(coll)$x()) == unlist(coll) + 1 )
	}
)
