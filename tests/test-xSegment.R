
message("xSegment")

forall(
	'segmenting an empty collection is identity',
	test_cases$positive_with_collection,
	xSegment(num, coll) %equals% as.list(coll)
)
