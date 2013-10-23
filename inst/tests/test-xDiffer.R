
context("xDiffer")

forall(
	"the set difference with an left empty collection is the empty list",
	G$standard$two_colls_right_empty(),
	expect = 
		xDiffer(coll1, coll2) %equals% list(),
	given =
		length(coll1) == 0
)

forall(
	"the set difference with an right empty collection is identiy for left",
	G$standard$two_colls_left_empty(),
	expect = 
		xDiffer(coll1, coll2) %equals% as.list(coll1),
	given =
		length(coll2) == 0
)

forall(
	"the set difference behaves the same as R's",
	G$standard$two_colls(),
	expect =
		xDiffer(coll1, coll2) %equals% as.list(setdiff(coll1, coll2)),
	given =
		length(coll1) > 0 && length(coll2)
)