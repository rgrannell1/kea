
# 1.
# xIsTrue is the safe way to check if a condition is true
# in an if statement:
#
# an unfortunate side-effect of R's vectorisation and
# three-value logic (True, False, Na) is that the values
# logical(0) or NA can pop up in unexpected places.
#
# xIsTrue guarantees one of two values are returned; true or false.
#
# this gets rid of the undesired error thrown if
# logical(0) or NA is returned.

if ( xIsTrue(logical(0)) ) {
	stop("this will not be executed")
}

if ( xIsTrue(NA) ) {
	stop("this will not be executed")
}

# either
#
# if (logical(0)) {}
#
# or
#
# if (NA) {}
#
# will break
