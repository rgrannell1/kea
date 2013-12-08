
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xWords('') %equals% character(0)
	)

message('arrow $ xWords')

