
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xCapture")

	forall(
		"const of a value is a function",
		test_cases$collection,
		xCapture(coll)() %equals% coll
	)

	forall(
		"const can swallow as many arguments as its given.",
		test_cases$collection,
		do.call( xCapture(1), as.list(coll) ) == 1
	)
