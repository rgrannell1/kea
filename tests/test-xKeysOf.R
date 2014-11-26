
kea ::: load_test_dependencies(environment())

unit_test("xKeysOf")

	over(coll) +

	it("keys of always returns character") +
	holdsWhen(
		suchThat $ is_collection(coll),

		is.character(xKeysOf(coll))
	) +

	it("keys of is character 0 for no names") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xKeysOf(coll) %is% character(0)
	) +

	it("keys of is names") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xKeysOf(coll) %is% names(coll)
	) +

	run()
