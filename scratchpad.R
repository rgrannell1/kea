
xVal(letterset, c(letters, LETTERS))

words <-
x_("/home/ryan/Desktop/war-and-peace.txt") $ xReadWords() $ xUniqueOf() $
xSelect(word := {
	all(xToChars(word) %in% letterset)
}) $
xUniqueOf()

words $ xTabulate() $ xSortBy(xSecondOf) $ xTake(10)



as_atomic <- function (coll, mode) {

}

as_atomic <- local({

	check_valid <- function (elem, mode) {
		if (length(elem) != 1) {
			stop("")
		}
		if (mode(elem) != mode) {
			stop("")
		}
	}

	function (coll, mode) {

		if (length(coll) == 0) {
			vector(mode)
		} else if (is.atomic(coll)) {

		} else {
			for (elem in coll) check_valid(elem, mode)
			as.vector(coll, mode = mode)
		}
	}
})

