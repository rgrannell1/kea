
# 1. Chunk a flat name,value .csv into pairs of names, values

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4,Hong Kong,42.8"

age_by_country <-
	x_(csv) $ xExplode(',') $
	xChunk(2) $ xMap( xUnspread((key : value) := {
		list(key, as.numeric(value))
	}) ) $
	xZipKeys()

# list(
#     Monaco = "48.9", Japan = "44.6", Italy = "44.3",
#     Germany = "43.7", Jersey = "43.4", `Hong Kong` = "42.8")

# 2. Select every third element in collection, starting from the
#    second value. (2nd, 5th, ...)

x_(letters) $ xChunk(3) $ xSelect(row := {
	# -- because the last might have less than three elements.
	xLenOf(row) == 3
}) $
xAtCol(2)

# list("b", "e", "h", "k", "n", "q", "t", "w")
