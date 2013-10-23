
context("xInter")

forall(
	"the intersection with an empty collection is an empty list",
	list(coll1 = G$collection_zero, coll2 = G$collection_zero),
	expect = 
		xInter(coll1, coll2) %equals% list(),
	given =
		length(coll1) == 0 || length(coll2) == 0
)

forall(
	"the intersection in general behaves the same as R's intersection",
	list(coll1 = G$collection(), coll2 = G$collection()),
	expect = 
		xInter(coll1, coll2) %equals% as.list(intersect(coll1, coll2)),
	given =
		length(coll1) > 0 || length(coll2) > 0
)
