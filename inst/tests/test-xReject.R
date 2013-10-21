
context("xReject: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xReject(fn, coll) %equals% list()
)

forall(
	"a truth function is list unit for collection.",
	G$standard$truth_with_coll(),
	expect = 
		xReject(fn, coll) %equals% list(),
	given = 
		length(coll) > 0
)

forall(
	"a falsity function is list identity for collection.",
	G$standard$falsity_with_coll(),
	xReject(fn, coll) %equals% coll
)

forall(
	"a na function is list identity for collection.",
	G$standard$mu_with_coll(),
	xReject(fn, coll) %equals% as.list(coll)
)

forall(
	"selecting the odd-numbers works as expected, and ordering is preserved.",
	G$standard$mod2_over_ints(),
	xReject(fn, coll) %equals% as.list(coll[coll %% 2 == 1])
)

forall(
	"collection.xReject selects odd-numbers.",
	G$standard$mod2_over_ints(),
	x_(coll)$xReject(fn)$x() %equals% 
		as.list(coll[coll %% 2 == 1])
)

forall(
	"function.xReject selects odd-numbers.",
	G$standard$mod2_over_ints(),
	x_(fn)$xReject(coll)$x() %equals% 
		as.list(coll[coll %% 2 == 1])
)
