
kea ::: load_test_dependencies(environment())

to_test       <- function (nums, coll) {}
body(to_test) <- kea ::: Must_All_Be_Indices(nums, coll)

message("Must_All_Be_Indices")

	over(coll) +

	it('positive indices are always indices of coll') +
	worksWhen(
		suchThat $ is_collection(coll),
		to_test(seq_along(coll), coll)
	) +

	it('negative indices are always indices of coll') +
	worksWhen(
		suchThat $ is_collection(coll),
		to_test(-seq_along(coll), coll)
	) +

	it('works for sampled indices') +
	worksWhen(
		suchThat $ is_collection(coll),
		{
			indices <- c(seq_along(coll), -seq_along(coll), 0)
			indices <- sample(indices, size = sample.int(length(indices), 1))

			to_test(indices, coll)
		}
	) +

	run()

	over(nums, coll) +

	it('always fails for infinites') +
	failsWhen(
		suchThat $ is_collection(coll) && is_collection(nums) && any(is.infinite( unlist(nums) )),
		to_test(nums, coll)
	) +

	run()
