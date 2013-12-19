
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xSetProd")

	forall(
		"setprod of any empty collection is empty list",
		test_cases$two_collections,
		xSetProd(list(coll1, coll2)) %equals% list(),
		given =
			prod(length(coll1), length(coll2)) == 0
	)

	forall(
		"the length is the product of the set length",
		test_cases$two_collections,
		length( xSetProd(list(coll1, coll2)) ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)

message("arrow $ xSetProd")

	forall(
		"collection $ xSetProd",
		test_cases$two_collections,
		length( x_(list(coll1, coll2))$xSetProd()$x() ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)

message("arrow $ x_SetProd")

	forall(
		"collection $ x_SetProd",
		test_cases$two_collections,
		length( x_(list(coll1, coll2))$x_SetProd() ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)

message("arrow $ xSetProd...")

	forall(
		"collection $ xSetProd...",
		test_cases$two_collections,
		length( x_(coll1)$xSetProd...(coll2)$x() ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)

message("arrow $ x_SetProd...")

	forall(
		"collection $ x_SetProd...",
		test_cases$two_collections,
		length( x_(coll1)$x_SetProd...(coll2) ) ==
		prod(length(coll1), length(coll2)),
		given =
			prod(length(coll1), length(coll2)) > 0
	)
