
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xImplode')

	forall(
		"collapsing with character() is the same as collapsing with ''",
		test_cases$str_words,
		xImplode('', strs) %equals% xImplode(character(0), strs)
	)

	forall(
		"collapsing character() and '' acts as identity ",
		test_cases$str_word_and_words,
		xImplode(str, strs) %equals% xImplode(str, strs[length(strs) != 0])
	)
