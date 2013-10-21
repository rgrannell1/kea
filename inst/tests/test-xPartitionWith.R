
context("xPartitionWith: positive controls")

forall(
	"the empty collection always yields the empty list.",
	list(fn = G$logical_functions, coll = G$collection_zero),
	xPartitionWith(fn, coll) %equals% list()
)

forall(
	"a truth function is [list collection, list unit].",
	G$standard$truth_with_coll(),
	expect = 
		xPartitionWith(fn, coll) %equals% 
			list( as.list(coll), list() ),
	given = 
		length(coll) > 0
)

forall(
	"a falsity function is [list unit, list collection].",
	G$standard$falsity_with_coll(),
	xPartitionWith(fn, coll) %equals% 
		list( list(), as.list(coll) ),
	given = 
		length(coll) > 0
)

forall(
	"a na function is [list unit, list collection].",
	G$standard$mu_with_coll(),
	xPartitionWith(fn, coll) %equals% 
		list( list(), as.list(coll) ),
	given = 
		length(coll) > 0
)

forall(
	"partitioning the integers by evenness works as expected, and ordering is preserved.",
	G$standard$mod2_over_ints(),
	xPartitionWith(fn, coll) %equals% 
		list( 
			as.list(coll[coll %% 2 == 0]), 
			as.list(coll[coll %% 2 == 1]) )
)

forall(
	"collection.xPartitionWith partitions into even and odd-numbers.",
	G$standard$mod2_over_ints(),
	x_(coll)$xPartitionWith(fn)$x() %equals% 
		list( 
			as.list(coll[coll %% 2 == 0]), 
			as.list(coll[coll %% 2 == 1]) )
)

forall(
	"function.xPartitionWith partitions into even and odd-numbers.",
	G$standard$mod2_over_ints(),
	x_(fn)$xPartitionWith(coll)$x() %equals% 
		list( 
			as.list(coll[coll %% 2 == 0]), 
			as.list(coll[coll %% 2 == 1]) )
)
