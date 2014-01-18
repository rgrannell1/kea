
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFirstOf')

forall("first always returns the first element of a collection",
	test_cases$collection,
	expect =
		xFirstOf(coll) %equals% coll[[1]],
	given =
		length(coll) >= 1
)

