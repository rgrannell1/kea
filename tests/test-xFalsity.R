
kea ::: load_test_dependencies(environment())

unit_test("xFalsity")

	over(val) +
	it('falsity always yields false') +
	holdsWhen(
		True,

		xFalsity(val) == False
	) +
	run()
