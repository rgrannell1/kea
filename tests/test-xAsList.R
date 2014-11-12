
kea ::: load_test_dependencies(environment())

message("xAsList")

	over(coll) +

	describe("as list is always a list") +
	holdsWhen(
		suchThat $ is_collection(coll),
		is.list(xAsList(coll)),
		length(xAsList(coll)) == length(coll)
	) +

	describe("names are kept") +
	holdsWhen(
		suchThat $ is_collection(coll),
		names(xAsList(coll)) %is% names(coll)
	) +

	run()
