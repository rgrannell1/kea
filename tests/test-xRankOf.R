
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xRankOf")

	forall(
		"rank of empty coll is int zero",
		test_cases$collection_zero,
		xRankOf(coll) %equals% integer(0)
	)

	forall(
		"rank of a sequence is the sequence",
		test_cases$num_positive_integer,
		xRankOf(1:num) %equals% 1:num
	)

	forall(
		"rank of a repeated numbers is sequence of equal length",
		test_cases$num_positive_integer,
		xRankOf(rep(num, num)) %equals% 1:num
	)
