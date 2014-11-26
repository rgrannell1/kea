
kea ::: load_test_dependencies(environment())

message("xAsList")

	over(coll) +

	it("as list is always a list") +
	holdsWhen(
		suchThat $ is_collection(coll),

		is.list(xAsList(coll)),
		length(xAsList(coll)) == length(coll)
	) +

	it("names are kept") +
	holdsWhen(
		suchThat $ is_collection(coll),

		names(xAsList(coll)) %is% names(coll)
	) +

	run()
