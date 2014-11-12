
kea ::: load_test_dependencies(environment())

message("xNot")

	over(val) +

	describe("a value is always equal to itself.") +
	holdsWhen(
		True,

		!xNot(val, val)
	) +

	run()

	over(val1, val2) +

	describe("always returns a logical value.") +
	holdsWhen(
		True,

		is.logical(xNot(val1, val2)),
		length(xNot(val1, val2)) == 1
	) +

	describe("xNot always runs") +
	worksWhen(
		True,

		xNot(val1, val2)
	) +

	run()
