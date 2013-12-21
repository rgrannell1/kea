
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
		length(xShuffle(coll)) == length(coll)
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
		length(x_(coll)$xShuffle()$x()) == length(coll)
	)

message("arrow $ xShuffle...")

	forall(
		"collection $ xShuffle...",
		test_cases$collection,
		length(x_(coll)$xShuffle...()$x()) == 1
	)

message("arrow $ x_Shuffle")

	forall(
		"collection $ x_Shuffle",
		test_cases$collection,
		length(x_(coll)$x_Shuffle()) == length(coll)
	)

message("arrow $ x_Shuffle...")

	forall(
		"collection $ x_Shuffle...",
		test_cases$collection,
		length(x_(coll)$x_Shuffle...()) == 1
	)
