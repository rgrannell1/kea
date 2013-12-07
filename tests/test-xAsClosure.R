
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAsClosure")

	forall(
		"the arity of the null function is null",
		xArity(function () Null) == 0
	)

message("arrow $ xAsClosure")

message("fn $ xAsClosure")

	forall(
		"the arity of the null function is null",
		x_(fn)$xAsClosure()
	)

