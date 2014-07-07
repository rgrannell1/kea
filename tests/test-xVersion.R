
kiwi ::: load_test_dependencies(environment())

message("xVersion")

	over(val) +

	describe('xVersion always gives the current version') +
	holdsWhen(
		True,
		paste0(xVersion(val), collapse = '.') == packageVersion("kiwi")
	) +

	run()
