
kea ::: load_test_dependencies(environment())

message("xFalsity")

	over(val) +
	it('falsity always yields false') +
	holdsWhen(
		True,

		xFalsity(val) == False
	) +
	run()
