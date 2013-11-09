
# It is difficult to give examples of do, since it
# is used for side effects. I've personally used it
# for unzipping every file in a directory, and for printing
# values in a collection.

# print some numbers for admiration.

xDo(print, 1:10)

# print the files in your current directory.

xDo(print %of% list.files, ".")

# Ask some questions without collecting the results (for some reason).

if ("velocity of an unladen swallow" == 100) {
	xDo(
		readline,
		c(
			"what is your name? ",
			"what is your favourite colour? ",
			"what is the capital of Assyria? "))
}
