
kiwi ::: load_test_dependencies(environment())

to_test       <- function (coll) {}
body(to_test) <- kiwi ::: Must_Be_Collection(coll)

message('Must_Be_Collection')

	over(coll) +

	describe('always passes for collections') +
	worksWhen(
		is_collection(coll),
		to_test(coll)
	) +

	run()

	over(coll) +

	describe('always fails for non-collections') +
	failsWhen(
		!is_collection(coll),
		to_test(coll)
	) +

	run()
