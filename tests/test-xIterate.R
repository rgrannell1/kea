
kea ::: load_test_dependencies()

message("xIterate")

	over(val) +

	describe("returning a val is val") +
	holdsWhen(
		True,

		xIterate(function (val) Return(val), val) %is% val
	) +

	run()
