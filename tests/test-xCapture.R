
kea ::: load_test_dependencies(environment())

message("xCapture")

	over(val) +

	it('xCapture always yields the same value.') +
	holdsWhen(
		True,

		xCapture(val)() %is% val
	) +

	run()

	over(val1, val2) +

	it('xCaptures result ignores input.') +
	holdsWhen(
		True,

		xCapture(val1)(val2) %is% val1
	) +

	run()
