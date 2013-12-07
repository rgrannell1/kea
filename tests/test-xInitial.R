
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xInit")

forall(
	"init of an empty collection always yields the empty list.",
	test_cases$collection,
	expect =
		xInit(coll) %equals% list(),
	given =
		length(coll) == 0
)

forall(
	"init of a list shortens the list by one",
	test_cases$collection,
	expect =
		length(xInit(coll)) == length(coll) - 1,
	given =
		length(coll) > 0
)
