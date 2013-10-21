
context("xSelect: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xSelect(fn, coll) %equals% list()
)

forall(
	"a truth function is list identity for collection.",
	list(fn = G$truth, coll = G$collection),
	expect = 
		xSelect(fn, coll) %equals% coll,
	given = 
		length(coll) > 0
)

forall(
	"a falsity function is list unit for collection.",
	list(fn = G$falsity, coll = G$collection),
	xSelect(fn, coll) %equals% list()
)

forall(
	"a na function is list unit for collection.",
	list(fn = G$mu, coll = G$collection),
	xSelect(fn, coll) %equals% list()
)

forall(
	"selecting the odd-numbers works as expected, and ordering is preserved.",
	list(
		fn = function () {
			function (x) x %% 2 == 0
		},
		coll = G$integers()
	),
	xSelect(fn, coll) %equals% as.list(coll[coll %% 2 == 0])
)

forall(
	"collection.xSelect works as expected.",
	list(
		fn = function () {
			function (x) x %% 2 == 0
		},
		coll = G$integers()
	),
	x_(coll)$xSelect(fn)$x() %equals% as.list(coll[coll %% 2 == 0])
)

forall(
	"function.xSelect works as expected.",
	list(
		fn = function () {
			function (x) x %% 2 == 0
		},
		coll = G$integers()
	),
	x_(fn)$xSelect(coll)$x() %equals% as.list(coll[coll %% 2 == 0])
)
