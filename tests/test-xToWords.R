
kea ::: load_test_dependencies(environment())

message('xToWords')

	over(str) +

	describe("works of empty string is empty vector") +
	holdsWhen(
		True,

		xToWords('') %is% character(0)
	) +

	run()
