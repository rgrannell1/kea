
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xCollapse')

	forall(
		"collapsing with character() is the same as collapsing with ''",
		test_cases$str_words,
		{
			xCollapse('', strs) %equals%
			xCollapse(character(0), strs)
		}
	)

	forall(
		"collapsing character() and '' acts as identity ",
		test_cases$str_word_and_words,
		{
			xCollapse(str, strs) %equals%
			xCollapse(str, strs[length(strs) != 0])
		}
	)

message('arrow $ xCollapse')

	forall(
		"collapsing with character() is the same as collapsing with ''",
		test_cases$str_words,
		x_(strs)$xCollapse('')$x() %equals%
		x_(strs)$xCollapse(character(0))$x()
	)

message('arrow $ xCollapse...')

message('arrow $ x_Collapse')

	forall(
		"collapsing with character() is the same as collapsing with ''",
		test_cases$str_words,
		x_(strs)$x_Collapse('') %equals%
		x_(strs)$x_Collapse(character(0))
	)
