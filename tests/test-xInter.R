
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xInter")

	forall(
		"the intersection with an empty collection is an empty list",
		test_cases$two_collection_zeros,
		xInter(coll1, coll2) %equals% list(),
		given =
			length(coll1) == 0 || length(coll2) == 0
	)

	#forall(
	#	"the intersection in general behaves the same as R's intersection",
	#	test_cases$two_collections,
	#	xInter(coll1, coll2) %equals% as.list(intersect(coll1, coll2)),
	#	given =
	#		length(coll1) > 0 || length(coll2) > 0
	#)

message("arrow $ xInter")
