
kea ::: load_test_dependencies(environment())

unit_test('xFix')

	over(val) +

	it("partially applying with no arguments is the original function") +
	holdsWhen(
		True,

		xFix(identity, list())(val) %is% identity(val)
	) +

	run()
