
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xAddKeys")

	forall(
		"unit is named list()",
		test_cases$str_words,
		xAddKeys(character(0), list()) %is% structure(list(), names = character(0))
	)

	forall(
		"same length names are correctly added",
		test_cases$str_words,
		all(names( xAddKeys(strs, seq_along(strs)) ) == strs)
	)


kiwi ::: load_test_dependencies(environment())

message("xAddKeys")

	over(coll1, coll2) +

	describe("addkeys with empty collections is named empty list") +
	holdsWhen(
		is_collection(coll1) && length(coll1) == 0 &&
		is_collection(coll2) && length(coll2) == 0,

		xAddKeys(coll1, coll2) %is% as_named(list())
	) +

	run()

	over(strs) +

	describe("names and values are correct") +
	holdsWhen(
		is.character(strs) && !anyNA(strs),

		all(names(xAddKeys(strs, strs)) == strs),
		all(unname( unlist(xAddKeys(strs, strs)) ) == strs)
	) +

	run()
