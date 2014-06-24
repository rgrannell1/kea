
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xThread (+)")

	over(val) +

	describe('threading with no function is that value') +
	when(
		TRUE,
		xThread(val, list()) %is% val
	) +

	run()

	over(nums) +

	describe(paste0(
		"threading a number through linear functions ",
		"is the product of the number with the function coefficients.", collapse = '\n'
	)) +
	when(
		is.numeric(nums) && is.finite(nums),
		{

			num = nums[[1]]

			linear_functions = lapply(nums, function (constant) {
				function (x) constant * x
			})

			xThread(num, linear_functions) == num * prod(nums)
		}
	) +

	run()
