
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xThirdOf')

forall("third always returns the third element of a collection",
	test_cases$collection,
	expect =
		xThirdOf(coll) %equals% coll[[3]],
	given =
		length(coll) >= 3
)

