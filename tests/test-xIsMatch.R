
kea ::: load_test_dependencies(environment())

unit_test('xIsMatch')

	over(rexp, str) +

	it("yields empty logical vector for empty regular expressions") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str),

		xIsMatch(rexp, str) %is% logical(0)
	) +

	it("yields true for match-all regular expressions") +
	holdsWhen(
		suchThat $ is_empty_collection(rexp) &&
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str),

		xIsMatch('', str),
		xIsMatch('.+', str)
	) +

	run()
