
# 1. Working with csv data.
#
# seperate a csv of country,median age pairs
# into a list of countries and ages.
# this is a nicer format for Arrow to work with
# than a string or data.frame.

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4"

x_(csv) $ xExplode(',') $ xChunk(2) $ xZip() $
x_AddKeys(c('country', 'age'))

# list(
#     country =
#     list(
#         "Monaco",
#         "Japan",
#         "Italy",
#         "Germany",
#         "Jersey"),
#     age =
#     list(
#         "48.9",
#         "44.6",
#         "44.3",
#         "43.7",
#         "43.4"
# ))

