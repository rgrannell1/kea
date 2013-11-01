
message("xPoll")

test_that("xPoll", {

	expect_that(
		xPoll(Truth, list()),
		equals(0)
	)
	expect_that(
		xPoll(Truth, 1:4),
		equals(4)
	)
	expect_that(
		xPoll(Falsity, 1:4),
		equals(0)
	)
	expect_that(
		xPoll(function (x) x %% 2 == 0, 1:4),
		equals(2)
	)

})
