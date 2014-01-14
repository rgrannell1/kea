
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xThird')

forall("third always returns the third element of a collection",
	test_cases$collection,
	expect =
		xThird(coll) %equals% coll[[3]],
	given =
		length(coll) >= 3
)

