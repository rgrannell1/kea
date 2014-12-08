
kea ::: load_test_dependencies(environment())

unit_test("xTruth")

	over(val) +

	it('xTruth always yields true') +
	holdsWhen(
		True,

		xTruth(val)
	) +

	run()
