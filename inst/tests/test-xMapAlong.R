
context("xMapAlong: positive controls")

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
	list(
		fn = function () {
			function (val, ind) val + 1
		},
		coll = G$integers()
	),
	all( unlist(xMapAlong(fn, coll)) == unlist(coll) + 1 )
)
