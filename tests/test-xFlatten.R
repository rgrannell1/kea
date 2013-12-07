
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xFlatten")

forall(
	"flattening an empty collection returns the empty list",
	list(n = G$positive(), coll = G$recursive_zero),
	xFlatten(n, coll) %equals% list()
)

forall(
	"flattening to 1 is unlist",
	list(n = G$positive(), coll = test_cases$collection, conv = G$to_recursive),
	xFlatten(n, conv(coll)) %equals% as.list(unlist(coll))
)

forall(
	"flattening to Inf is the identity",
	list(coll = test_cases$collection, conv = G$to_recursive),
	xFlatten(Inf, conv(coll)) %equals% as.list(coll)
)

message("arrow $ xFlatten")

forall(
	"coll$xFlatten",
	list(coll = test_cases$collection, conv = G$to_recursive),
	x_(conv(coll))$xFlatten(Inf) %equals% as.list(coll)
)
