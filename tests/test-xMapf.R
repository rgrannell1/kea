
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xFmap")

forall(
	"partmapping over an empty list is the empty list",
	G$standard$coll(),
	xFmap(function (x) x^2)(coll) %equals% as.list(),
	given =
		length(coll) == 0
)

forall(
	"partmapping identity across a collection is the list identity",
	G$standard$coll(),
	xFmap(identity)(coll) %equals% as.list(coll),
	given =
		length(coll) > 0
)

forall(
	"partmapping increment over integers works",
	G$standard$inc_over_ints(),
	all( xFmap(fn)(coll) == unlist(coll) + 1 )
)

message("xFmap: x_()")

forall(
	"function.xFmap increments over integers",
	G$standard$inc_over_ints(),
	all( (x_(fn)$xFmap()$x())(coll) == unlist(coll) + 1 )
)
