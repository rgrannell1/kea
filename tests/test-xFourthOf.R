
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xFourthOf')

forall("fourth always returns the fourth element of a collection",
	test_cases$collection,
	expect =
		xFourthOf(coll) %equals% coll[[4]],
	given =
		length(coll) >= 4
)
