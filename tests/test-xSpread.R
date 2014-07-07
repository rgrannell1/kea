
kiwi ::: load_test_dependencies(environment())

forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xSpread")

	forall(
		"identity as variadic collects a list of arguments.",
		test_cases$num_integer,
		xSpread(identity)(num, num) %is% list(num, num)
	)

	over(coll) +

	describe("applying to a spread function is identity") +
	holdsWhen(
		is_collection(coll) && !is_named(coll),
		do.call(xSpread(identity), as.list(coll)) %is% as.list(coll)
	) +

	run()
