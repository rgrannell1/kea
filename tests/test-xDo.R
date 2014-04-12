
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

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
