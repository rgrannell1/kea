

kea ::: load_test_dependencies(environment())

message("xVersion")

	over(val) +

	it('xVersion always gives the current version') +
	holdsWhen(
		True,

		paste0(xVersion(val), collapse = '.') == packageVersion("kea")
	) +

	it('xVersion always is an integer') +
	holdsWhen(
		True,

		is.integer(xVersion(val))
	) +

	run()
