
kiwi ::: load_test_dependencies(environment())

message('xToLines')

	over(strs) +

	describe("works of empty string is empty vector") +
	holdsWhen(
		True,
		xToLines('') %is% character(0)
	) +

	run()
