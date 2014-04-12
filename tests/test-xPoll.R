
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xPoll")

	forall(
		"polling the empty collection returns integer(0)",
		test_cases$collection_zero,
		xPoll(Truth, coll) %equals% integer(0))

	forall(
		"polling with truth returns length coll",
		test_cases$collection,
		xPoll(Truth, coll) == length(coll),
		given =
			length(coll) > 0)

	forall(
		"polling with falsity returns 0",
		test_cases$collection,
		xPoll(Falsity, coll) == 0,
		given =
			length(coll) > 0)

	forall(
		"polling with moot returns 0",
		test_cases$collection,
		xPoll(Moot, coll) == 0,
		given =
			length(coll) > 0)

	forall(
		"polling counts true occurrences",
		test_cases$mod2_over_ints,
		xPoll(fn, coll) == length(which(coll %% 2 == 0)),
		given =
			length(coll) > 0)
