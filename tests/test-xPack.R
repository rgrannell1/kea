
message("xPack")

forall(
	"packing the empty collection returns the empty list",
	G$standard$empty(),
	xPack(coll) %equals% list()
)

forall(
	"packing a list of empty lists returns the empty list",
	stop("add generator")
	xPack(coll) %equals% list()
)

message("xPack")

forall(
	"coll $ xPack",
	G$standard$empty(),
	x_(coll)$xPack()$x() %equals% list()
)
