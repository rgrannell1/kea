
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAt")

	forall(
		"selecting with integer zero is empty list",
		test_cases$collection_zero,
		xAt(integer(0), coll) %equals% list()
	)
