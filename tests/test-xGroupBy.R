
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xGroupBy")

	over(val, coll) +

	describe("grouping an empty collection is list()") +
	holdsWhen(
		is_collection(coll) && length(coll) == 0,
		xGroupBy(identity, coll) %is% list()
	) +

	describe("grouping one value gives a group with that key, that val under identity") +
	holdsWhen(
		True,
		xGroupBy(identity, list(val)) %is% list( list(val, list(val)) )
	) +

	describe("a unique set groups as itself, with itself as keys under identity") +
	holdsWhen(
		is_collection(coll),
		{

			set <- as.list(unique(coll))
			group <- xGroupBy(identity, set)

			keys <- lapply( group, function (x) x[[1]] )
			vals <- lapply( group, function (x) x[[2]][[1]] )

			keys %is% set && vals %is% set
		}
	) +

	run()
