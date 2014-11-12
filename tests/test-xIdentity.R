
kea ::: load_test_dependencies()

message("xIdentity")

	over(val) +

	describe('xIdentity always yields its input') +
	holdsWhen(
		True,

		xIdentity(val) %is% val
	) +

	run()
