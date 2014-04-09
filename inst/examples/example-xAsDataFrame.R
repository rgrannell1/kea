
# 1. Convert a text csv to a data frame.
#
# Data from Hadley Wickham's repository `data-stride`
#

raw_cocaine_seizure_data <-
"
WA,77,217,1,5000
CT,51,248,1,4800
FL,68,43,1,3500
OH,69,123,1,3500
MS,75,118,1,3400
VA,73,127,1,3000
FL,54,50,1,3000
IL,58,140,1,2800
GA,77,127,1,2600
MS,49,74,1,2600
LA,78,82,1,2600
LA,78,81,1,2600
LA,74,79,1,2600
ME,81,12,1,2500
"
x_(raw_cocaine_seizure_data) $ xToLines () $
xMap(row := {
	row <- xExplode(',', row )

	xJoin...( xFirstOf(row), as.double(xRestOf(row)) )
}) $
xZip() $
xAddKeys(c('state', 'potency', 'weight', 'month', 'price')) $ x_AsDataFrame()

#
# data.frame(
#     state   = c("WA", "CT", "FL", "OH", "MS", "VA", "FL", "IL", "GA", "MS", "LA", "LA", "LA", "ME"),
#     potency = c(77, 51, 68, 69, 75, 73, 54, 58, 77, 49, 78, 78, 74, 81),
#     weight  = c(217, 248, 43, 123, 118, 127, 50, 140, 127, 74, 82, 81, 79, 12),
#     month   = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#     price   = c(5000, 4800, 3500, 3500, 3400, 3000, 3000, 2800, 2600, 2600, 2600, 2600, 2600, 2500)
# )
