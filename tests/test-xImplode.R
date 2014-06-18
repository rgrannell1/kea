
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message('xImplode')

	over(strs) +

	describe('collapsing with character 0 is character 0') +
	when(
		is.character(strs),
		xImplode(character(0), strs) %is% character(0)
	) +

	run()
