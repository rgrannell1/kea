
context("xMapWhen")

test_that("xMapWhen", {

	expect_equal(
		xMapWhen(function (x) x %% 2 == 0, function (x) x + 1, 1:5),
		list(1, 3, 3, 5, 5)
	)
	expect_equal(
		xMapWhen(function (x) True, function (x) x + 1, 1:5),
		as.list(2:6)
	)
	expect_equal(
		xMapWhen(function (x) False, function (x) x + 1, 1:5),
		as.list(1:5)
	)
})
