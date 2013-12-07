
message("xPack")

forall(
	"packing the empty collection returns the empty list",
	test_cases$collection_zero,
	xPack(coll) %equals% list()
)

forall(
	"packing a list of empty lists returns the empty list",
	test_cases$collection_of_length_zero,
	xPack(coll) %equals% list()
)

message("xPack")

forall(
	"coll $ xPack",
	G$standard$empty(),
	x_(coll)$xPack()$x() %equals% list()
)
