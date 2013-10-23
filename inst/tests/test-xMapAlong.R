
context("xMapAlong")

forall(
	"mapalong the empty collection always yields the empty list.",
	list(
		fn = function () {
			function (val, ind) val + ind 
		}, 
		coll = G$collection_zero
	),
	xMapAlong(fn, coll) %equals% list()
)

forall(
	"mapalong's indices are correct",
	list(
		fn = function () {
			function (val, ind) ind
		},
		coll = G$collection
	),
	xMapAlong(fn, coll) %equals% as.list(seq_along(coll))
)

forall(
	"mapalong's values are correct",
	list(
		fn = function () {
			function (val, ind) val
		},
		coll = G$collection
	),
	xMapAlong(fn, coll) %equals% as.list(coll)
)

forall(
	"mapalong's can increment correctly.",
	G$standard$inc2_over_ints,
	all( unlist(xMapAlong(fn, coll)) == unlist(coll) + 1 )
)


context("arrow $ xMapAlong")

forall(
	"collection.xMapAlong increments correctly.",
	G$standard$inc2_over_ints,
	{
		all( unlist(x_(coll)$xMapAlong(fn)$x()) == unlist(coll) + 1 )
	}
)

forall(
	"function.xMapAlong increments correctly.",
	G$standard$inc2_over_ints,
	{
		all( unlist(x_(fn)$xMapAlong(coll)$x()) == unlist(coll) + 1 )
	}
)





