
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xWords')

	forall(
		"words of the empty string is an empty vector",
		list(),
		xWords('') %equals% character(0)
	)

message('arrow $ xWords')

	forall(
		"collection $ xWords",
		list(),
		x_('')$xWords()$x() %equals% character(0)
	)

message('arrow $ x_Words')

	forall(
		"collection $ x_Words",
		list(),
		x_('')$x_Words() %equals% character(0)
)
