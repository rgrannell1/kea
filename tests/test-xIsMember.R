

	forall <- arrow:::forall
	test_cases <- arrow:::test_cases

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
				letter <- sample(letters, size = 1)
				!xIsMember(toupper(letter), coll)
			},
			given =
				length(coll) > 0
		)

	message("arrow $ xIsMember")

		forall(
			"collection $ xIsMember",
			test_cases$letters,{
				letter <- sample(letters, size = 1)
				!x_(coll)$xIsMember(toupper(letter))$x()
			},
			given =
				length(coll) > 0
		)
	message("arrow $ x_IsMember")

		forall(
			"collection $ xIsMember",
			test_cases$letters,{
				letter <- sample(letters, size = 1)
				!x_(coll)$x_IsMember(toupper(letter))
			},
			given =
				length(coll) > 0
		)
