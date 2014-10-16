
# Must_Be_Longer_Than
#
# length is known to be a number; it is given as a literal.
# coll is known to be a collection.s
#

Must_Be_Longer_Than <- function (LENGTH, COLL) {

	COLL   <- substitute(COLL)
	LENGTH <- substitute(LENGTH)

	bquote( if (length( .(COLL) ) <= .(LENGTH) )  {

		message <-
			"The argument matching " %+% ddquote( .(COLL) ) %+%
			" must have more than " %+%  .(LENGTH) %+% " elements.\n" %+%
			"The actual length was " %+% length(.(COLL)) %+% "." %+%
			summate( .(COLL) )

		throw_kea_error(sys.call(), message)
	} )

}
