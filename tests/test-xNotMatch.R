
kea ::: load_test_dependencies(environment())

unit_test('xNotMatch')

	over(coll, str) +

	it("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xNotMatch(coll, str) %is% logical(0)
	) +

	it("all strings match the empty string") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		is_character(str) && length(str) == 1 && !is.na(str),

		!xNotMatch('', str),
		!xNotMatch('.+', str)
	) +

	run()
