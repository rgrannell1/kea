
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message("xVersion (+)")

	over(val) +

	describe('xVersion always gives the current version') +
	when(
		True,
		paste0(xVersion(val), collapse = '.') == packageVersion("arrow")		
	) +

	run()
	