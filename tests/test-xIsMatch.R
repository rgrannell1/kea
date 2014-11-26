
kea ::: load_test_dependencies(environment())

message('xIsMatch')

	over(coll, str) +

	describe("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na)(str),

		xIsMatch(coll, str) %is% logical(0)
	) +

	describe("all strings match the empty string") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na)(str),

		xIsMatch('', str),
		xIsMatch('.+', str)
	) +

	run()
