
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xMapIndexed")

	forall(
		"mapindexed the empty collection always yields the empty list.",
		list(
			fn = function () {
				function (val, ind) val + ind
			},
			coll = G$collection_zero
		),
		xMapIndexed(fn, coll) %equals% list()
	)

	forall(
		"mapindexed's indices are correct",
		list(
			fn = function () {
				function (val, ind) ind
			},
			coll = G$collection
		),
		xMapIndexed(fn, coll) %equals% as.list(seq_along(coll))
	)

	forall(
		"mapindexed's values are correct",
		list(
			fn = function () {
				function (val, ind) val
			},
			coll = G$collection
		),
		xMapIndexed(fn, coll) %equals% as.list(coll)
	)

	forall(
		"mapindexed's can increment correctly.",
		G$standard$inc2_over_ints,
		all( unlist(xMapIndexed(fn, coll)) == unlist(coll) + 1 )
	)

message("arrow $ xMapIndexed")

	forall(
		"collection $ xMapIndexed increments correctly.",
		G$standard$inc2_over_ints,
		{
			all( unlist(x_(coll)$xMapIndexed(fn)$x()) == unlist(coll) + 1 )
		}
	)

	forall(
		"function $ xMapIndexed increments correctly.",
		G$standard$inc2_over_ints,
		{
			all( unlist(x_(fn)$xMapIndexed(coll)$x()) == unlist(coll) + 1 )
		}
	)
