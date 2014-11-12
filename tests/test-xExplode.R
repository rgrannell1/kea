
kea ::: load_test_dependencies()

message('xExplode')

	over(str) +

	describe('splitting empty collection is empty collection') +
	holdsWhen(
		is_character(str) && length(str) == 1 &&
		!is.na(str) && is_alphanumeric(unlist(str)),

		xExplode(str, character(0)) %is% character(0)
	) +

	describe("exploding the empty string is empty string") +
	holdsWhen(
		is_character(str) && length(str) == 1 &&
		!is.na(str) && is_alphanumeric(unlist(str)),

		xExplode(str, '') %is% ''
	) +

	run()
