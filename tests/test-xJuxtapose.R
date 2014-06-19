
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xJuxtapose")

	forall(
		"juxtaposing preserves identities within the list",
		test_cases$num_positive_integer,
		xJuxtapose(list(identity))(num) %is% list(num)
	)

	forall(
		"juxtaposing can apply multiple functions",
		test_cases$num_positive_integer,
		xJuxtapose(list(identity, identity))(num) %is% list(num, num)
	)

	forall(
		"juxtaposing works with incrementing",
		test_cases$succ_over_integers,
		xJuxtapose(list(fn))(coll) %is% list(coll + 1)
)

