
kea ::: load_test_dependencies(environment())

over(colls) +

it("product of an empty collection is an empty collection") +
holdsWhen(
	suchThat $ is_empty_collection(colls),

	xProdSetOf(colls) %is% list()
) +

run()
