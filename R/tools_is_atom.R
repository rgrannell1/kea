

# For Testing Use Only; too slow for real use.
#
#
#

is_numeric <- function (coll) {
	tryDefault({
		xAsDouble(coll)
		TRUE
	}, FALSE) ||

	tryDefault({
		xAsInteger(coll)
		TRUE
	}, FALSE)

}

is_integer <- function (coll) {
	tryDefault({
		xAsInteger(coll)
		TRUE
	}, FALSE)
}

is_double <- function (coll) {
	tryDefault({
		xAsDouble(coll)
		TRUE
	}, FALSE)
}

is_character <- function (coll) {
	tryDefault({
		xAsCharacter(coll)
		TRUE
	}, FALSE)
}

is_logical <- function (coll) {
	tryDefault({
		xAsLogical(coll)
		TRUE
	}, FALSE)
}

is_complex <- function (coll) {
	tryDefault({
		xAsComplex(coll)
		TRUE
	}, FALSE)
}

is_raw <- function (coll) {
	tryDefault({
		xAsRaw(coll)
		TRUE
	}, FALSE)
}
