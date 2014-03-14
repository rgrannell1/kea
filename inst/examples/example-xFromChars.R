
# 1. Remove Specific Characters from a string
#    in all likelyhood I'd use the base function gsub() here,
#    but xFromChars and xReject work too.

x_('the letter e is overrated') $
xToChars() $
xReject(
	char := {
		char == 'e'
}) $
x_FromChars()
