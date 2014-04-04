
# 1. turn a csv of country names and median ages into
#    a named list of countires, with values being the median age.

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4"

x_(csv) $ xExplode(',') $ xChunk(2) $ x_ZipKeys()

# list(
#    Monaco =
#        48.9,
#    Japan =
#        44.6,
#    Italy =
#        44.3,
#    Germany =
#        43.7,
#    Jersey =
#        43.4
# )

x_(csv) $ xExplode(',') $ xChunk(2) $ x_ZipKeys()
