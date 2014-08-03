
kea ::: load_test_dependencies(environment())

over(colls) +

describe("product of an empty collection is an empty collection") +
holdsWhen(
	is_collection(colls) && length(colls) == 0,
	xProdSetOf(colls) %is% list()
) +

run()
