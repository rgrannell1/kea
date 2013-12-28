
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xUnwords')

	forall(
		"xUnlines of character(0) is character(0)",
		list(0),
		xUnwords(character(0)) %equals% character(0)
	)

message('arrow $ xUnwords')
