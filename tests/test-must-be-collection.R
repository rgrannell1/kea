
kea ::: load_test_dependencies(environment())

to_test       <- function (coll) {}
body(to_test) <- kea ::: Must_Be_Collection(coll)

message('Must_Be_Collection')

	over(coll) +

	it('always passes for collections') +
	worksWhen(
		suchThat $ is_collection(coll),
		to_test(coll)
	) +

	run()

	over(coll) +

	it('always fails for non-collections') +
	failsWhen(
		suchThat $ not_collection(coll),
		to_test(coll)
	) +

	run()
