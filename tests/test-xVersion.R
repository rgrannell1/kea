
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xVersion")

	forall(
		"check that the version matches the current version",
		list(),
		paste0(xVersion(), collapse = '.') == packageVersion("arrow")
	)
