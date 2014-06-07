
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xThread")

	forall(
		"threading one value is that value",
		test_cases$num_integer,
		xThread(num, list()) == num
	)

	forall(
		"threading one value with identities is that value",
		test_cases$num_integer,
		xThread(num, list(
			identity,
			identity,
			identity,
			identity
		)) == num
	)
