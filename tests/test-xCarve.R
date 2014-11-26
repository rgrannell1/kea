
kea ::: load_test_dependencies(environment())

message('xCarve')

	over(str, nums) +

	describe('selecting at zero is character(0)') +
	holdsWhen(
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str) &&
		suchThat $ is_empty_collection(nums),

		xCarve(0, str) %is% character(0),
		xCarve(nums, str) %is% character(0)
	) +

	describe('slicing with indices is identity') +
	holdsWhen(
		and_(suchThat $ is_singleton_character, suchThat $ not_na_collection)(str),

		{

			indices <- seq_len(nchar( unlist(str) ))
			xCarve(indices, str) %is% unlist(str)
		}
	) +

	run()
