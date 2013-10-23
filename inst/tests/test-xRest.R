
context("xRest")

forall(
	"xRest of an empty collection always yields the empty list.",
	list(coll = G$collection),
	expect =
		xRest(coll) %equals% list(),
	given =
		length(coll) == 0 
)

forall(
	"xRest of a list shortens the list by one (usually)",
	list(coll = G$collection),
	expect =
		length(xRest(coll)) == length(coll) - 1,
	given =
		length(coll) > 0 
)
