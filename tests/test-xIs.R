
kiwi ::: load_test_dependencies(environment())

message("xIs (+)")

	over(val) +

	describe("a value is always equal to itself.") +
	holdsWhen(
		True,
		xIs(val, val)
	) +

	run()

	over(val1, val2) +

	describe("always returns a logical value.") +
	holdsWhen(
		True,
		is.logical(xIs(val1, val2)),
		length(xIs(val1, val2)) == 1
	) +

	run()
