
message("xShuffle")

forall(
	"shuffling the empty collection returns the empty list",
	list(coll = G$collection_zero),
	xShuffle(coll) %equals% list()
)

forall(
	"shuffling preserves length",
	list(coll = G$collection()),
	length(xShuffle(coll)) %equals% length(coll)
)

forall(
	"shuffling returns a list",
	list(coll = G$collection()),
	is.list(xShuffle(coll))
)

message("arrow $ xShuffle")

forall(
	"shuffling preserves length",
	list(coll = G$collection()),
	length(x_(coll)$xShuffle()$x()) %equals% length(coll)
)
