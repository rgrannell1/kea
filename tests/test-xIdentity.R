
kiwi ::: load_test_dependencies(environment())

message("xIdentity (+)")

	over(val) +
	describe('xIdentity always yields its input') +
	when(
		True,
		xIdentity(val) %is% val
	) +
	run()
