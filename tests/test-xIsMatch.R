
kea ::: load_test_dependencies(environment())

message('xIsMatch')

	over(rexp, str) +

	describe("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na)(str),

		xIsMatch(rexp, str) %is% logical(0)
	) +

	describe("all strings match matchall regexps") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na)(str),

		xIsMatch('', str),
		xIsMatch('.+', str)
	) +

	run()
