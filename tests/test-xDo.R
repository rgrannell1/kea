
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xDo")

	forall(
		"do always returns null",
		test_cases$collection,
		is.null( xDo(identity, coll) )
	)

	forall(
		"do actually calls a side-effectful function",
		test_cases$num_positive_integer,
		capture.output(xDo(cat, 1:num)) == paste0(1:num, collapse = '')
	)
