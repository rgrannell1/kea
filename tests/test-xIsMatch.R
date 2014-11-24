
kea ::: load_test_dependencies(environment())

message('xIsMatch')

	over(coll, str) +

	describe("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xIsMatch(coll, str) %is% logical(0)
	) +

	describe("all strings match the empty string") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xIsMatch('', str),
		xIsMatch('.+', str)
	) +

	run()
