
message("xLocater")

test_that('xLocater', {

	isEven <- function (n) {
		n %% 2 == 0
	}

	expect_equal(xLocater(Truth, list()), integer(0))
	expect_equal(xLocater(Falsity, 1:10), integer(0))
	expect_equal(xLocater(Truth, 1:10), 10)
	expect_equal(xLocater(isEven, 1:10), 10)

})
