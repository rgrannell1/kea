
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xArityOf (+)")

	over(fn) +

	describe("xArityOf is always a nonnegative number") +
	holdsWhen(
		is.function(fn),
		xArityOf(fn) >= 0
	) +

	run()

