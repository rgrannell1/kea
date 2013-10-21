
context("xSelect: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xSelect(fn, coll) %equals% list()
)

forall(
	"a truth function is list identity for collection.",
	G$standard$truth_with_coll(),
	expect = 
		xSelect(fn, coll) %equals% coll,
	given = 
		length(coll) > 0
)

forall(
	"a falsity function is list unit for collection.",
	G$standard$falsity_with_coll(),
	xSelect(fn, coll) %equals% list()
)

forall(
	"a na function is list unit for collection.",
	G$standard$mu_with_coll(),
	xSelect(fn, coll) %equals% list()
)

forall(
	"selecting the even-numbers works as expected, and ordering is preserved.",
	G$standard$mod2_over_ints(),
	xSelect(fn, coll) %equals% as.list(coll[coll %% 2 == 0])
)

forall(
	"collection.xSelect selects even-numbers.",
	G$standard$mod2_over_ints(),
	x_(coll)$xSelect(fn)$x() %equals% 
		as.list(coll[coll %% 2 == 0])
)

forall(
	"function.xSelect selects even-numbers.",
	G$standard$mod2_over_ints(),
	x_(fn)$xSelect(coll)$x() %equals% 
		as.list(coll[coll %% 2 == 0])
)
