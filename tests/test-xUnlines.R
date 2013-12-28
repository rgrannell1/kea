
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnlines')

	forall(
		"xUnlines of character(0) is character(0)",
		list(0),
		xUnlines(character(0)) %equals% character(0)
	)

message('arrow $ xUnlines')

