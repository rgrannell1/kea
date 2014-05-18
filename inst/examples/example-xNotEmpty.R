
# 1. Conditionally execute code on non-empty collections.
#
# Often code on lists will have two branches;
# the case for the empty list and the case for the
# non-empty list.

safeFirstOf <- function (coll, default = NA) {
	if (xNotEmpty(coll)) {
		xFirstOf(coll)
	} else {
		default
	}
}

safeFirstOf(letters)

# 'a'

safeFirstOf(character(0))

# Na - xFirstOf usually fails for empty collections.
