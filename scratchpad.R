
xVal(letterset, c(letters, LETTERS))

x_("/home/ryan/Desktop/war-and-peace.txt") $
xReadWords() $ xUniqueOf() $
xSelect(word := {
	all(xToChars(word) %in% letterset)
}) $
xUniqueOf() $ xSortBy(nchar) $
xReverse() $ xTake(10)



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









blood_glucose <- 135

patients_data <-
xZip...(
	'mg per dl (fasting)' =
		c(130, 137, 139, 140, 141, 137, 127, 143, 135, 127,
			77, 79, 79, 79, 88, 87, 95, 82, 93, 94),
	'category' =
		xJoin...(xRepeat(10, 'diabetic'), xRepeat(10, 'non-diabetic'))
)

k_nearest <- (dist : point : space) := {
	# classification.
	# given a space of points to test against,
	# guess the category of a point based on its nearest neighbours.

	k <- floor( sqrt(xLenOf(space)) )

	dist_to_test_point <- xPartial...(dist, point)

	distances <-
		x_(space) | xPluck('mg per dl (fasting)') |
		xMap(dist_to_test_point) | x_AsDouble()

	# rank the distances from closest to furthest.
	nearest <- x_(space) | xSlice(xWhere(xRankOf(distances) <= k))

	# get the frequencies of the categories among the closest.
	category_frequencies <-
		nearest | xPluck('category') |
		xTabulate() | x_SortBy(xSecondOf)

	# select the name of the most frequent category
	xFirstOf(xFirstOf(category_frequencies))
}

k_nearest(
	(x1 : x2) := abs(x1 - x2), blood_glucose, patients_data)