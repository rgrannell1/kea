
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xSliceString')

	forall(
		"selecting at zero returns the character(0).",
		list(),
		xSliceString('', 0) %is% character(0)
	)
