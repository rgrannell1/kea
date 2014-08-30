
kea ::: load_test_dependencies(environment())

message("methods")

	over(val) +

	describe("calling a nullary method works") +
	holdsWhen(
		True,

		x_(val) $ x_I() %is% val
	) +

	run()

	over(val) +

	describe("calling an unevaluated variable works") +
	holdsWhen(
		True,

		x_(val) $ xIs(val) $ x_I(),
		x_(val) $ x_Is(val)

	) +

	run()

