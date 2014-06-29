
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xFlatMap")

	forall(
		"flatmapping increment increments the list",
		test_cases$succ_over_integers,
		all( unlist(xFlatMap(fn, coll)) == unlist(coll) + 1 )
	)

	forall(
		"flatmapping can extend a collection length.",
		test_cases$integers,
		length(xFlatMap(function (x) c(x, x), coll)) == 2 * length(coll)
	)


kiwi ::: load_test_dependencies(environment())


message("xFlatMap")

	over(fn, coll) +

	describe("flatmap of empty collection is always empty") +
	holdsWhen(
		is.function(fn) && is_collection(coll) && length(coll) == 0,
		xFlatMap(fn, coll) %is% list()
	) +

	describe("flatmap with identity is the coll") +
	holdsWhen(
		is_collection(coll),
		xFlatMap(identity, coll) %is% as.list(coll)
	) +

	run()
