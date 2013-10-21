
context("xMap: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xMap(fn, coll) %equals% list()
)

forall(
	"mapping identity over the list preserves its contents & length.",
	list(coll = G$collection),
	xMap(identity, coll) %equals% as.list(coll) &&
	length(xMap(identity, coll)) == length(coll)
)

forall(
	"mapping increment increments the list",
	list(
		fn = function () {
			function (x) x + 1
		}, 
		coll = G$integers()),
	all( unlist(xMap(fn, coll)) == unlist(coll) + 1 )
)
