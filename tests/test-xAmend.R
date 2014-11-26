
kea ::: load_test_dependencies(environment())

message('xAmend')

	over(coll, str) +

	it("character 0 matches all strings") +
	holdsWhen(
		suchThat $ is_empty_collection(coll) &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xAmend(coll, coll, coll) %is% character(0),
		xAmend(coll, coll, str)  %is% character(0),
		xAmend(coll, str, str)   %is% character(0),
		xAmend(coll, str, coll)  %is% character(0)
	) +

	it("replacing each character with none is empty") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		unname(xAmend('.', '', str)) == ''
	) +

	it("at least for now, xAmend keeps the input name") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		identical(names(xAmend('.', str, str)), names(str))
	) +

	run()
