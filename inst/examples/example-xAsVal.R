
# 1. xAsVal can be used to 'lock' a list of constant variables
#    so it can't accidentally be updated.

consts <- list(
	pi = 3.14,
	golden = 1.618
)

xAsVal(consts)

# consts$pi <- 2
# would throw an error.

