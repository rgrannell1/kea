
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xParamsOf (+)")

	over(fn) +

	describe("always returns character") +
	holdsWhen(
		is.function(fn),
		is.character(xParamsOf(fn))
	) +

	describe("works for non-primitive functions") +
	holdsWhen(
		is.function(fn) && !is.primitive(fn),
		( xParamsOf(fn) %is% names(formals(fn)) ) ||
		( xParamsOf(fn) %is% character(0) )
	) +

	run()
