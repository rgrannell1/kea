
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xFirst')

forall("first always returns the first element of a collection",
	list(coll = G$collection),
	expect =
		xFirst(coll) %equals% coll[[1]],
	given =
		length(coll) <= 1
)
