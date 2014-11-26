
kea ::: load_test_dependencies(environment())

unit_test('xIsMatch')

	over(rexp, str) +

	it("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str),

		xIsMatch(rexp, str) %is% logical(0)
	) +

	it("all strings match matchall regexps") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str),

		xIsMatch('', str),
		xIsMatch('.+', str)
	) +

	run()
