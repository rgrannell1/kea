
kiwi ::: load_test_dependencies(environment())

message("xCapture (+)")

	over(val) +
	describe('xCapture always yields the same value.') +
	when(
		True,
		xCapture(val)() %is% val
	) +
	run()

	over(val1, val2) +
	describe('xCaptures result ignores input.') +
	when(
		True,
		xCapture(val1)(val2) %is% val1
	) +
	run()
