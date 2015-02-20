
kea ::: load_test_dependencies(environment())

unit_test("xAmassBy")

	over(val, coll) +

	it("grouping an empty collection is list()") +
	holdsWhen(
		suchThat $ is_empty_collection(coll),

		xAmassBy(identity, coll) %is% list()
	) +

	it("grouping one value gives a group with that val under identity") +
	holdsWhen(
		True,

		xAmassBy(identity, list(val)) %is% list( list(val) )
	) +

	it("a unique set groups as itself, with itself as keys under identity") +
	holdsWhen(
		suchThat $ is_collection(coll),

		{

			set <- as.list(unique(coll))
			group <- xAmassBy(identity, set)

			group %is% lapply(set, list)
		}
	) +

	run()
