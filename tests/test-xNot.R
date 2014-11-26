
kea ::: load_test_dependencies(environment())

unit_test("xNot")

	over(val) +

	it("a value is always equal to itself.") +
	holdsWhen(
		True,

		!xNot(val, val)
	) +

	run()

	over(val1, val2) +

	it("always returns a logical value.") +
	holdsWhen(
		True,

		is.logical(xNot(val1, val2)),
		length(xNot(val1, val2)) == 1
	) +

	it("xNot always runs") +
	worksWhen(
		True,

		xNot(val1, val2)
	) +

	run()
