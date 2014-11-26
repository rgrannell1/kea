
kea ::: load_test_dependencies(environment())

message("xAddKeys")

	over(coll0, coll1) +

	it("two empty collections is named empty") +
	holdsWhen(
		is_collection(coll0) && is_collection(coll1) &&
		length(coll0) == 0 && length(coll1) == 0,

		xAddKeys(coll0, coll1) %is% as_named(list())
	) +

	run()

	over(strs) +

	it("names of collection is strs") +
	holdsWhen(
		is_character(strs) && length(strs) > 0 && !anyNA(strs),

		all(names( xAddKeys(strs, seq_along(strs)) ) == unlist(strs))
	) +

	run()
