
kea ::: load_test_dependencies()

message('xToWords')

	over(str) +

	describe("works of empty string is empty vector") +
	holdsWhen(
		True,

		xToWords('') %is% character(0)
	) +

	run()
