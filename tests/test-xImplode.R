
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xImplode (+)')

	over(strs) +

	describe('collapsing with character 0 is character 0') +
	holdsWhen(
		is.character(strs),
		xImplode(character(0), strs) %is% character(0)
	) +

	run()
