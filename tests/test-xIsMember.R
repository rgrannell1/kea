
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsMember")

	forall(
		"xIsMember of the empty set is logical(0)",
		test_cases$nonnegative_with_collection_zero,
		xIsMember(num, coll) %equals% logical(0)
	)

	forall(
		"xIsMember with an element of the set is always true",
		test_cases$letters,
		xIsMember(sample(coll, size = 1), coll),
		given =
			length(coll) > 0
	)

	forall(
		"xIsMember with an element outside of the set is always false",
		test_cases$letters,{
			letter <- rsample(letters, size = 1)
			!xIsMember(toupper(letter), coll)
		},
		given =
			length(coll) > 0
	)
