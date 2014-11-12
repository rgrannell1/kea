
kea ::: load_test_dependencies()

message("xKeysOf")

	over(coll) +

	describe("keys of always returns character") +
	holdsWhen(
		suchThat $ is_collection(coll),

		is.character(xKeysOf(coll))
	) +

	describe("keys of is character 0 for no names") +
	holdsWhen(
		suchThat $ not_named_collection(coll),

		xKeysOf(coll) %is% character(0)
	) +

	describe("keys of is names") +
	holdsWhen(
		suchThat $ is_named_collection(coll),

		xKeysOf(coll) %is% names(coll)
	) +

	run()
