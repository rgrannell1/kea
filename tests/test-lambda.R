
message(":=")

test_that(":=", {

	expect_that(
		(x := {x})(1),
		equals(1)
	)
	expect_that(
		(name := {
			paste0(name)
		})(10),
		equals('10')
	)
	expect_that(
		lapply(1:3, x := x^2),
		equals(list(1, 4, 9))
	)

	# closing over works
	k <- x := {
		y := {
			x
		}
	}
	expect_that(k(10)('no!'), equals(10))

})
