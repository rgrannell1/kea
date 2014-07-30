
kiwi ::: load_test_dependencies(environment())

message('xNotMatch')

	over(coll, str) +

	describe("character 0 matches all strings") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xNotMatch(coll, str) %is% logical(0)
	) +

	describe("all strings match the empty string") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 &&
		is_character(str) && length(str) == 1 && !is.na(str),

		!xNotMatch('', str),
		!xNotMatch('.+', str)
	) +

	run()
