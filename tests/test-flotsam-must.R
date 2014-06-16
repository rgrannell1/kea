
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

as_function <- function (macro) {

	out       <- function (x) {}
	body(out) <- macro(x)
	out
}

message("Must $ Be_Whole (+)")

	over(num) +

	describe('Be_Whole never fails for numbers') +
	when(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) && round(num) == num,
		as_function(Must $ Be_Whole)(num)
	) +

	run()
