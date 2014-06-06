
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xAt")

	forall(
		"indexing a letter with xAt selects the letter",
		test_cases$letters_and_index,
		xAt(num, coll) == coll[[num]]
	)
