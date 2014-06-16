
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xImplode')

	forall(
		"collapsing with character() is the same as collapsing with ''",
		test_cases$str_words,
		xImplode('', strs) %is% xImplode(character(0), strs)
	)

	forall(
		"collapsing character() and '' acts as identity ",
		test_cases$str_word_and_words,
		xImplode(str, strs) %is% xImplode(str, strs[length(strs) != 0])
	)
