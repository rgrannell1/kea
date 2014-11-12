
kea ::: load_test_dependencies(environment())

message("xArityOf")

	over(fn) +

	describe("xArityOf is always a nonnegative number") +
	holdsWhen(
		suchThat $ is_function(fn),

		xArityOf(fn) >= 0
	) +

	run()

