
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xSelect")

	over(coll) +
	describe("the empty collection always yields the list") +
	when(
		length(coll) == 0,
		xSelect(function (x) True, coll)  %equals% list(),
		xSelect(function (x) False, coll) %equals% list(),
		xSelect(function (x) Na, coll)    %equals% list()) +
	run()

	forall(
		"a truth function is list identity for collection.",
		test_cases$truth_with_coll,
		xSelect(fn, coll) %equals% as.list(coll),
		given =
			length(coll) > 0
	)

	forall(
		"a falsity function is list unit for collection.",
		test_cases$falsity_with_coll,
		xSelect(fn, coll) %equals% list()
	)

	forall(
		"a na function is list unit for collection.",
		test_cases$moot_with_coll,
		xSelect(fn, coll) %equals% list()
	)

	forall(
		"selecting the even-numbers works as expected, and ordering is preserved.",
		test_cases$mod2_over_ints,
		xSelect(fn, coll) %equals% as.list(coll[coll %% 2 == 0])
	)
