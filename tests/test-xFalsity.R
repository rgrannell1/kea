
kea ::: load_test_dependencies()

message("xFalsity")

	over(val) +
	describe('falsity always yields false') +
	holdsWhen(
		True,

		xFalsity(val) == False
	) +
	run()
