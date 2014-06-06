
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xLift")

	forall(
		'lifting two identites with plus is plus',
		test_cases$num_positive_integer,
		xLift('+', list(identity, identity))(num) == num + num
	)
