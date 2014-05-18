
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message('xSecondOf')

forall("second always returns the second element of a collection",
	test_cases$collection,
	expect =
		xSecondOf(coll) %equals% coll[[2]],
	given =
		length(coll) >= 2
)
