
kea ::: load_test_dependencies(environment())

message('xToLines')

	over(str) +

	it("works of empty string is empty vector") +
	holdsWhen(
		True,

		xToLines('') %is% character(0)
	) +

	run()
