
kea ::: load_test_dependencies()

over(colls) +

describe("product of an empty collection is an empty collection") +
holdsWhen(
	suchThat $ is_empty_collection(colls),

	xProdSetOf(colls) %is% list()
) +

run()
