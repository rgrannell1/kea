
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSecond')

forall("second always returns the second element of a collection",
	list(coll = G$collection),
	expect =
		xSecond(coll) %equals% coll[[2]],
	given =
		length(coll) <= 2
)
