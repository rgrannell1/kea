
kea ::: load_test_dependencies(environment())





message("Values")

	over(val) +

	describe("logical values have expected values.") +
	holdsWhen(
		TRUE,

		True  == TRUE,
		False == FALSE,
		is.na(Na),
		is.null(Null)
	) +

	 run()





message("summate")

	summate <- kea ::: summate

	over(val) +

	describe("summate should never fail.") +
	worksWhen(
		True,
		summate(val)
	) +

	describe("summate is always length-one") +
	holdsWhen(
		True,
		length(summate(val)) == 1
	) +

	run()




message(" %+% ")

	'%+%' <- kea ::: `%+%`

	over(str1, str2) +

	describe("string joining works") +
	holdsWhen(
		is.character(str1) && is.character(str2) &&
		length(str1) == 1 && length(str2) == 1,

		(str1 %+% str2) == paste0(str1, str2)
	) +

	run()





message("ddquote")






message("pluralise")

	pluralise <- kea ::: pluralise

	stopifnot(pluralise('', 0) == 's')

	over(str, num) +

	describe("pluralising adds an s") +
	holdsWhen(
		is.numeric(num) && !is.na(unlist(num)) &&
		(length(num) == 0 || (length(num) == 1 && round(unlist(num)) == num & num >= 1)),

		pluralise('', num) == 's'
	) +

	run()





message("is_na")

	over(val) +

	describe("na values are na.") +
	holdsWhen(

		is_na(NA),
		is_na(NA_integer_),
		is_na(NA_complex_),
		is_na(NA_real_),
		is_na(NA_character_)
	) +

	describe("is false for non-length ones") +
	holdsWhen(
		length(val) != 1,
		!is_na(val)
	) +

	describe("is_na whenever na") +
	holdsWhen(
		is_atomic(val) && is.na(val) && !is.nan(val),
		is_na(val)
	) +

	run()




	message("elem_is_na")
