
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xPoll")

	forall(
		"polling the empty collection returns 0",
		test_cases$collection_zero,
		xPoll(Truth, coll) == 0)

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
		xPoll(fn, coll) == length(which(coll %% 2 == 0)) )


message("arrow $ xPoll")

	forall(
		"function $ xPoll",
		test_cases$mod2_over_ints,
		x_(fn)$xPoll(coll)$x() == length(which(coll %% 2 == 0))
	)

	forall(
		"collection $ xPoll",
		test_cases$mod2_over_ints,
		x_(coll)$xPoll(fn)$x() == length(which(coll %% 2 == 0))
	)

message("arrow $ xPoll...")

message("arrow $ x_Poll")

	forall(
		"function $ x_Poll",
		test_cases$mod2_over_ints,
		x_(fn)$x_Poll(coll) == length(which(coll %% 2 == 0))
	)

	forall(
		"collection $ x_Poll",
		test_cases$mod2_over_ints,
		x_(coll)$x_Poll(fn) == length(which(coll %% 2 == 0))
	)