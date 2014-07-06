
kiwi ::: load_test_dependencies(environment())

to_test       <- function (nums, coll) {}
body(to_test) <- kiwi ::: Must_All_Be_Indices(nums, coll)

message("Must_All_Be_Indices")

	over(coll) +

	describe('indices are always indices of coll') +
	worksWhen(
		is_collection(coll),
		to_test(seq_along(coll), coll)
	) +

	run()

	over(nums, coll) +

	describe('always fails for infinites') +
	failsWhen(
		any(is.infinite(nums)) && is_collection(coll),
		to_test(nums, coll)
	) +

	run()
