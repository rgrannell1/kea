

# For Testing Use Only; too slow for real use.
#
#
#

is_numeric <- function (coll) {
	tryDefault({
		xAsDouble(coll)
		True
	}, False) ||

	tryDefault({
		xAsInteger(coll)
		True
	}, False)

}

is_integer <- function (coll) {
	tryDefault({
		xAsInteger(coll)
		True
	}, False)
}

is_double <- function (coll) {
	tryDefault({
		xAsDouble(coll)
		True
	}, False)
}

is_character <- function (coll) {
	tryDefault({
		xAsCharacter(coll)
		True
	}, False)
}

is_logical <- function (coll) {
	tryDefault({
		xAsLogical(coll)
		True
	}, False)
}

is_complex <- function (coll) {
	tryDefault({
		xAsComplex(coll)
		True
	}, False)
}

is_raw <- function (coll) {
	tryDefault({
		xAsRaw(coll)
		True
	}, False)
}
