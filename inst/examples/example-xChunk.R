
# 1. Chunk a flat name,value .csv into pairs of names, values

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4,Hong Kong,42.8"

age_by_country <-
	x_(csv) $ xExplode(',') $
	xChunk(2) $ xMapply((key : value) := {
		list(key, as.numeric(value))
	}) $
	xZipKeys()

# list(
#     Monaco = "48.9", Japan = "44.6", Italy = "44.3",
#     Germany = "43.7", Jersey = "43.4", `Hong Kong` = "42.8")
