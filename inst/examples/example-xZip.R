
# 1. Working with csv data.
#
# seperate a csv of country,median age pairs
# into a list of countries and ages.
# this is a nicer format for Kea to work with
# than a string or data.frame.

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4"

x_(csv) $ xExplode(',') $ xChunk(2) $ xZip() $ x_AddKeys(c('country', 'age'))

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




# 2. is a string a palindrome?

palindromic <- "Marge let a moody baby doom a telegram."

# the letters in the string, right to left.
left_to_right <-
	x_(palindromic) $ xToChars() $ xMap(tolower) $ x_Select(xIsIn(coll = letters))

# the same string, reversed.
right_to_left <- x_(left_to_right) $ x_Reverse()

# or simply zip the two lists next to each other, and check
# if both elements of the pair are equal.

x_(xZip_(left_to_right, right_to_left)) $ xMap(xApply(xIs)) $ x_AllOf(xI)
