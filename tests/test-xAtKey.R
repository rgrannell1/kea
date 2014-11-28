
kea ::: load_test_dependencies(environment())

unit_test("xAtKey")

	over(str, coll) +

	it("fails for unnamed collections") +
	failsWhen(
		suchThat $ is_character(str) &&
		suchThat $ not_named_collection(coll),

		xAtKey(str, coll)
	) +

	it("fails for collections without that name") +
	failsWhen(
		suchThat $ is_character(str) &&
		suchThat $ is_named_collection(coll) &&
		!any(str == names(coll)),

		xAtKey(str, coll)

	) +

	run()
