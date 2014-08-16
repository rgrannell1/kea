
kea ::: load_test_dependencies(environment())





message("Logical Values")

	over(val) +

	describe("logical values have expected values.") +
	holdsWhen(
		TRUE,

		True  == TRUE,
		False == False,
		is.na(Na),
		is.null(Null)
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
