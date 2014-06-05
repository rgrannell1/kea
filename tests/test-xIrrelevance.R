
arrow ::: load_test_dependencies(environment())

message("xIrrelevance (+)")

	over(val) +
	describe('xIrrelevance always yields false') +
	when(
		True,
		is.na(xIrrelevance(val))
	) +
	run()
