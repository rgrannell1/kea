
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xDo")

	forall(
		"do always returns null",
		test_cases$collection,
		is.null( xDo(identity, coll) )
	)
