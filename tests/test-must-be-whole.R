
kiwi ::: load_test_dependencies(environment())


Must_Be_Whole <- kiwi ::: Must_Be_Whole

as_function <- function (macro) {

	out        <- function (x) {}
	body(out)  <- macro(x)
	environment <- loadNamespace('kiwi')
	out
}

message("Must_Be_Whole")

	over(num) +

	describe('Be_Whole never fails for numbers') +
	holdsWhen(
		is.numeric(num) && length(num) == 1 && !is.infinite(num) && round(num) == num,
		as_function(Must_Be_Whole)(num)
	) #+

	#run()

message("Must_Be_Whole")

	over(num) +

	describe('Be_Whole fails for NA') +
	failsWhen(
		is.numeric(num) && length(num) == 1 && is.na(num),
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for NaN') +
	failsWhen(
		is.numeric(num) && length(num) == 1 && is.nan(num),
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for non-rounded numbers') +
	failsWhen(
		is.numeric(num) && length(num) == 1&& !is.infinite(num) && round(num) != num,
		as_function(Must_Be_Whole)(num)
	) +

	describe('Be_Whole fails for length != numbers')

	#+

	#run()
