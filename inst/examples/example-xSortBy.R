
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
	xZip_(
		'mg per dl (fasting)' =
			c(130, 137, 139, 140, 141, 137, 127, 143, 135, 127,
				77, 79, 79, 79, 88, 87, 95, 82, 93, 94),
		'category' =
			xJoin_(xRepeat(10, 'diabetic'), xRepeat(10, 'non-diabetic'))
	)

# shuffle the rows of the data.
patients_data <- xShuffle(patients_data)

k_nearest <- (dist : point : training) := {
	# classification.
	# given training points to test against,
	# classify a point based on its nearest neighbours.

	k <- floor( sqrt(xLenOf(training)) )

	dist_to_test_point <- xFix_(dist, point)

	distances <-
		x_(training) $ xPluck('mg per dl (fasting)') $
		xMap(dist_to_test_point) $ x_AsDouble()

	# rank the distances from closest to furthest.
	nearest <- x_(training) $ xSlice(xWhere(xRankOf(distances) <= k))

	# get the frequencies of the categories among the closest.
	category_frequencies <-
		nearest $ xPluck('category') $ xTabulate() $ x_SortBy(xSecondOf)

	# select the name of the most frequent category
	xFirstOf(xFirstOf(category_frequencies))
}

k_nearest(
	(x1 : x2) := abs(x1 - x2), blood_glucose, patients_data)

# 'diabetic'

# 2. CE Get the N longest lines
#    text from Wikipedia

paragraph <-
"The plant glutamate cysteine ligase (GCL) is a redox-sensitive homodimeric enzyme, conserved in the plant kingdom.
[11] In an oxidizing environment, intermolecular disulfide bridges are formed and the enzyme switches to the dimeric active state.
 The midpoint potential of the critical cysteine pair is -318 mV.
 In addition to the redox-dependent control is the plant GCL enzyme feedback inhibited by GSH.
 [12] GCL is exclusively located in plastids, and glutathione synthetase is dual-targeted to plastids and cytosol, thus are GSH and gamma-glutamylcysteine exported from the plastids.
 [13] Both glutathione biosynthesis enzymes are essential in plants; knock-outs of GCL and GS are lethal to embryo and seedling."

x_(paragraph) $ xToLines() $ xSortBy(nchar) $ x_Take(3)

# list(
#     " [12] GCL is exclusively located in plastids, and glutathione synthetase is dual-targeted to plastids and cytosol, thus are GSH and gamma-glutamylcysteine exported from the plastids.",
#     "[11] In an oxidizing environment, intermolecular disulfide bridges are formed and the enzyme switches to the dimeric active state.",
#     " [13] Both glutathione biosynthesis enzymes are essential in plants; knock-outs of GCL and GS are lethal to embryo and seedling."
# )

# 3. CE How many ways do numbers drawn from an
#    array sum to zero?

nums <- c(-2, 2, -4, 2, -2, -6)

x_(xProdSetOf_(nums, nums, nums, nums)) $
xSelect(
	xs := {
		(unlist %then% sum)(xs) == 0
	}
) $
x_UniqueOf()

# list(
#     list(2, 2, -2, -2), list(2, -2, 2, -2),
#     list(-2, 2, 2, -2), list(2, -2, -2, 2),
#     list(-2, 2, -2, 2), list(-2, -2, 2, 2),
#     list(-6, 2, 2, 2),  list(2, -6, 2, 2),
#     list(2, 2, -6, 2),  list(2, 2, 2, -6)
# )
