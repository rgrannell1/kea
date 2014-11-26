
kea ::: load_test_dependencies(environment())

unit_test('xToWords')

	over(str) +

	it("works of empty string is empty vector") +
	holdsWhen(
		True,

		xToWords('') %is% character(0)
	) +

	run()
