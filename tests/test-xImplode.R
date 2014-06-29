
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('xImplode (+)')

	over(strs) +

	describe('collapsing with character 0 is character 0') +
	holdsWhen(
		is.character(strs) && !any(is.na(strs)),
		xImplode(character(0), strs) %is% character(0)
	) +

	describe('xImplode should never crash') +
	worksWhen(
		is.character(str) && is.character(strs) && !any(is.na(strs)),
		xImplode(str, strs)
	) +

	run()
