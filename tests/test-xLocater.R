
message("xLocatel")

forall(
	"the empty collection yields integer(0)",
	list(pred = G$logical_functions, coll = G$collection_zero),
	xLocater(pred, coll) %equals% integer()
)

forall(
	"a false function yields integer(0)",
	list(pred = G$falsity, coll = G$collection()),
	xLocater(pred, coll) %equals% integer()
)

forall(
	"a true function yields length(coll)",
	list(pred = G$truth, coll = G$collection()),
	xLocater(pred, coll) %equals% length(coll),
	given =
		length(coll) > 0
)
