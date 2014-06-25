
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

Must_Be_Whole <- kiwi ::: Must_Be_Whole

as_function <- function (macro) {

	out        <- function (x) {}
	body(out)  <- macro(x)
	environment <- loadNamespace('kiwi')
	out
}

message("Must_Be_Whole (+)")

	over(num) +

	describe('Be_Whole never fails for numbers') +
	when(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) && round(num) == num,
		as_function(Must_Be_Whole)(num)
	) #+

	#run()

message("Must_Be_Whole (-)")

	over(num) +

	describe('Be_Whole fails for NA') +
	failswhen(
		is.numeric(num) && length(num) == 1 && is.na(num),
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for NaN') +
	failswhen(
		is.numeric(num) && length(num) == 1 && is.nan(num),
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for non-rounded numbers') +
	failswhen(
		is.numeric(num) && length(num) == 1&& !is.infinite(num) && round(num) != num,
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for length != numbers')

	#+

	#run()
