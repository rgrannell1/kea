
message("xLocatel")

forall(
	"the empty collection yields integer(0)",
	list(pred = G$logical_functions, coll = G$collection_zero),
	xLocate(pred, coll) %equals% integer()
)

forall(
	"a false function yields integer(0)",
	list(pred = G$falsity, coll = G$collection()),
	xLocate(pred, coll) %equals% integer()
)

forall(
	"a true function yields 1",
	list(pred = G$truth, coll = G$collection()),
	xLocate(pred, coll) %equals% 1,
	given =
		length(coll) > 0
)

message("arrow $ xLocatel")

forall(
	"fn $ xLocatel",
	list(coll = G$collection()),
	x_(identity)$xLocatel(coll) %equals% coll
)

forall(
	"coll $ xLocatel",
	list(coll = G$collection()),
	x_(coll)$xLocatel(identity) %equals% coll
)
