
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xVectorise")

forall(
	"partmapping over an empty list is the empty list",
	G$standard$coll(),
	xVectorise(function (x) x^2)(coll) %equals% as.list(),
	given =
		length(coll) == 0
)

forall(
	"partmapping identity across a collection is the list identity",
	G$standard$coll(),
	xVectorise(identity)(coll) %equals% as.list(coll),
	given =
		length(coll) > 0
)

forall(
	"partmapping increment over integers works",
	G$standard$inc_over_ints(),
	all( xVectorise(fn)(coll) == unlist(coll) + 1 )
)

message("xVectorise: x_()")

forall(
	"function.xVectorise increments over integers",
	G$standard$inc_over_ints(),
	all( (x_(fn)$xVectorise()$x())(coll) == unlist(coll) + 1 )
)
