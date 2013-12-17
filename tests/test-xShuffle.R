
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xShuffle")

	forall(
		"shuffling the empty collection returns the empty list",
		test_cases$collection_zero,
		xShuffle(coll) %equals% list()
	)

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(xShuffle(coll)) %equals% length(coll)
	)

	forall(
		"shuffling returns a list",
		test_cases$collection,
		is.list(xShuffle(coll))
	)

message("arrow $ xShuffle")

message("collection $ xShuffle")

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(x_(coll)$xShuffle()$x()) %equals% length(coll)
	)

message("collection $ xShuffle...")

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(x_(coll)$xShuffle...()$x()) %equals% 1
	)

message("collection $ x_Shuffle")

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(x_(coll)$x_Shuffle()) %equals% length(coll)
	)

message("collection $ x_Shuffle...")

	forall(
		"shuffling preserves length",
		test_cases$collection,
		length(x_(coll)$x_Shuffle...()) %equals% 1
	)
