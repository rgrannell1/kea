\name{xVal}
\alias{xVal}
\title{xVal}
\usage{
xVal(sym, val)
}
\arguments{
  \item{sym}{a symbol or string.}

  \item{val}{an arbitrary value.}
}
\value{
Null; this function is used for side-effects.
}
\description{
Assign a constant value to the calling environment.
}
\details{
xVal works like normal R assignment, with the exception
that if any attempt to update the assigned variable will
result in an error. This is analogous to const in other
languages.
}
\section{Corner Cases}{
  Overwrites the value referenced by \code{sym}r if the
  variable already exists in the parent frame.
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

	k <- floor( sqrt(xLenOf(space)) )

	distances <- x_(space) $ xMap(
		point2 := dist( point, xFirstOf(point2))
	) $
	x_AsInteger()

	# rank the distances from closest to furthest.
	rankings <- rank(distances, ties = 'first')

	# get the frequencies of the categories among the closest.
	category_frequencies <-
	x_(space[ rankings <= k ]) $ xPluck('category') $
	xFlatten(1) $ xAsCharacter() $
	x_Tap(table \%then\% as.list)

	# finally, get the name of the most frequent category.
	names(category_frequencies[ which.max(category_frequencies) ])
}

k_nearest(
	(x1 : x2) := abs(x1 - x2), blood_glucose, patients_data)

# 'diabetic'
}
\seealso{
Other immutable_value_functions: \code{\link{xAsVal}};
\code{\link{xAsVar}}
}

