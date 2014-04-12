
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xAt")

	forall(
		"indexing a letter with xAt selects the letter",
		test_cases$letters_and_index,
		xAt(num, coll) == coll[[num]]
	)
