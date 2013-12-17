
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAsFunction")

	forall(
		"collection zero yields the empty list.",
		test_cases$collection_zero,
		xAsFunction(coll)(0) %equals% list()
	)

	forall(
		"single value indexing works.",
		test_cases$letters_and_index,
		xAsFunction(coll)(num) %equals% list(coll[num])
	)

	forall(
		"multi value indexing works.",
		test_cases$letters_and_indices,
		xAsFunction(coll)(nums) %equals% as.list(coll[nums])
	)

message("arrow $ xAsFunction")

message("collection $ xAsFunction")

	forall(
		"collection zero yields the empty list.",
		test_cases$collection_zero,
		x_(coll)$xAsFunction()$x()(0) %equals% list()
	)

message("collection $ xAsFunction...")

	forall(
		"collection zero yields the empty list.",
		test_cases$collection_zero,
		xAsFunction...(coll)(0) %equals% list()
	)

message("collection $ x_AsFunction")

	forall(
		"collection zero yields the empty list.",
		test_cases$collection_zero,
		x_(coll)$x_AsFunction()(0) %equals% list()
	)

message("collection $ x_AsFunction...")
