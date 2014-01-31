
# 1. k-nearest neighbours
#   is a patient diabetic?

# ------------- observations ------------- #
#
# the patient in question has a fasting blood glucose level
# of 135 mL / dL, and we also have data on the blood sugar
# of diabetic and non-diabetic individuals.
#
# By finding the most similar blood glucose levels and checking
# whether the people with those readings were or weren't diabetic.

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
	# given a space of points to test against,
	# guess the category of a point based on its nearest neighbours.

	k <- floor( sqrt(xLenOf(space)) )

	distances_to_point <- x_(space) $ xMap(
		point2 := dist( point, xFirstOf(point2))
	) $
	x_AsInteger()

	# rank the distances from closest to furthest.
	nearest <- space[xRank(distances_to_point) <= k]

	# get the frequencies of the categories among the closest.
	category_frequencies <-
	x_(nearest) $ xPluck('category') $
	xFlatten(1) $ xAsCharacter() $
	xTabulate() $
	x_SortBy(
		(a : b) := xSecondOf(b) > xSecondOf(b)
	)

	# select the name of the most frequent category
	xFirstOf(xFirstOf(category_frequencies))
}

k_nearest(
	(x1 : x2) := abs(x1 - x2), blood_glucose, patients_data)

# 'diabetic'

