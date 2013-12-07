
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xCombos")

	forall(
		"combos of empty collection is empty list",
		test_cases$nonnegative_with_collection_zero,
		xCombos(num, coll) %equals% list()
	)


message("xCombos...")


message("arrow $ xCombos")



message("arrow $ xCombos...")



