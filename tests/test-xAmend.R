
kea ::: load_test_dependencies(environment())

message('xAmend')

	over(coll, str) +

	describe("character 0 matches all strings") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0 &&
		is_character(str) && length(str) == 1 && !is.na(str),

		xAmend(coll, coll, coll) %is% character(0),
		xAmend(coll, coll, str)  %is% character(0),
		xAmend(coll, str, str)   %is% character(0),
		xAmend(coll, str, coll)  %is% character(0)
	) +

	describe("replacing each character with none is empty") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		unname(xAmend('.', '', str)) == ''
	) +

	describe("at least for now, xAmend keeps the input name") +
	holdsWhen(
		is_character(str) && length(str) == 1 && !is.na(str),

		identical(names(xAmend('.', str, str)), names(str))
	) +

	run()
