
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xIdentity")

	forall(
		"identity returns the collection.",
		test_cases$collection,
		xIdentity(coll) %equals% coll
	)
