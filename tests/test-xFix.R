
kea ::: load_test_dependencies()

message('xFix')

	over(val) +

	describe("partially applying with no arguments is the original function") +
	holdsWhen(
		True,

		xFix(identity, list())(val) %is% identity(val)
	) +

	run()
