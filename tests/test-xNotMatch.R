
kiwi ::: load_test_dependencies(environment())

message('xNotMatch')

	over(str, coll) +

	describe('character zero is logical zero') +
	holdsWhen(
		is.character(str) &&
		is_collection(coll) && length(coll) == 0 &&
		!is.na(str),
		xNotMatch(coll, str) %is% logical(0),
		xNotMatch(str, coll) %is% logical(0)
	) +

	describe("'' matches everything") +
	holdsWhen(
		is.character(str) && length(str) == 1,
		!xNotMatch('', str)
	) +

	run()
