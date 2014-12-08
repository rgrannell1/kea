
kea ::: load_test_dependencies(environment())

unit_test("xIterate")

	over(val) +

	it("returning a val is val") +
	holdsWhen(
		True,

		xIterate(function (val) Return(val), val) %is% val
	) +

	run()
