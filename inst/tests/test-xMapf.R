
context("xPartMap")

forall(
	"partmapping over an empty list is the empty list",
	G$standard$coll(),
	xPartMap(function (x) x^2)(coll) %equals% as.list(),
	given = 
		length(coll) == 0
)

forall(
	"partmapping identity across a collection is the list identity",
	G$standard$coll(),
	xPartMap(identity)(coll) %equals% as.list(coll),
	given = 
		length(coll) > 0
)

forall(
	"partmapping increment over integers works",
	G$standard$inc_over_ints(),
	all( xPartMap(fn)(coll) == unlist(coll) + 1 )
)

context("xPartMap: x_()")

forall(
	"function.xPartMap increments over integers",
	G$standard$inc_over_ints(),
	all( (x_(fn)$xPartMap()$x())(coll) == unlist(coll) + 1 )
)
