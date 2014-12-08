
kea ::: load_test_dependencies(environment())

unit_test("xIrrelevance")

	over(val) +

	it('xIrrelevance always yields false') +
	holdsWhen(
		True,

		is.na(xIrrelevance(val))
	) +

	run()
