
kiwi ::: load_test_dependencies(environment())

message("xFalsity (+)")

	over(val) +
	describe('falsity always yields false') +
	when(
		True,
		xFalsity(val) == False
	) +
	run()
