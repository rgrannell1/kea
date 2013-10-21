
context("xInit: positive controls")

forall(
	"init of an empty collection always yields the empty list.",
	list(coll = G$collection),
	expect =
		xInit(coll) %equals% list(),
	given =
		length(coll) == 0 
)

forall(
	"init of a list shortens the list by one (usually)",
	list(coll = G$collection),
	expect =
		length(xInit(coll)) == length(coll) - 1,
	given =
		length(coll) > 0 
)
