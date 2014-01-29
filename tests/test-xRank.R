
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xRank")

	forall(
		"rank of empty coll is int zero",
		test_cases$collection_zero,
		xRank(coll) %equals% integer(0)
	)

	forall(
		"rank of a sequence is the sequence",
		test_cases$num_positive_integer,
		xRank(1:num) %equals% 1:num
	)
