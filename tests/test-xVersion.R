
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xVersion (+)")

	over(val) +

	describe('xVersion always gives the current version') +
	holdsWhen(
		True,
		paste0(xVersion(val), collapse = '.') == packageVersion("kiwi")
	) +

	run()
