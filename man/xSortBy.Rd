\name{xSortBy}
\alias{xSortBy}
\alias{xSortBy...}
\title{xSortBy}
\usage{
xSortBy(pred, coll)

xSortBy...(pred, ...)
}
\arguments{
  \item{pred}{a binary predicate. is the left argument
  \bold{larger} than the right argument?}

  \item{coll}{a collection. The collection to sort.}

  \item{...}{see above.}
}
\value{
A list
}
\description{
Sort a collection using a predicate function.
}
\details{
\bold{xSortBy} allows a collection to be sorted by a custom
comparison operation. A classic example is sorting a
collection of collections (analogous to a data frame) by a
particular column.

\code{coll <- list(list('key1', 10), list('key2', 12),
list('key3', 0))}

\code{xSortBy((row1 : row2) := xSecondOf(row1) >
xSecondOf(row2), coll)}

\code{list(list("key2", 12), list("key1", 10), list("key3",
0))}

In the above example several rows of collection of
collections are rearranged by the value in one of their
columns.

\bold{xSortBy} is currently (v0.1.0) inefficient for large
collections, since both recursive and in-place sorting
algorithms are not suitable for use in R.
}
\section{Corner Cases}{
  If \bold{pred} returns Na it is interpreted as false; the
  right argument is larger than the left.
}
\examples{

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

# 2. CE Get the N longest lines
#    text from Wikipedia

paragraph <-
"The plant glutamate cysteine ligase (GCL) is a redox-sensitive homodimeric enzyme, conserved in the plant kingdom.
[11] In an oxidizing environment, intermolecular disulfide bridges are formed and the enzyme switches to the dimeric active state.
 The midpoint potential of the critical cysteine pair is -318 mV.
 In addition to the redox-dependent control is the plant GCL enzyme feedback inhibited by GSH.
 [12] GCL is exclusively located in plastids, and glutathione synthetase is dual-targeted to plastids and cytosol, thus are GSH and gamma-glutamylcysteine exported from the plastids.
 [13] Both glutathione biosynthesis enzymes are essential in plants; knock-outs of GCL and GS are lethal to embryo and seedling."

x_(paragraph) $
xToLines() $
xSortBy((line1 : line2) := {
	nchar(line2) > nchar(line1)
}) $
x_Take(3)

# 3. CE How many ways do numbers drawn from an
#    array sum to zero?

nums <- c(-2, 2, 1, -1, 4, -4, 2, -2, -6, 3, 5, 5, 7, 1)

x_(xProdSetOf...(nums, nums, nums, nums)) $
xSelect(
	xs := {
		(unlist \%then\% sum)(xs) == 0
	}
) $
xUniqueOf()
}
\seealso{
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

