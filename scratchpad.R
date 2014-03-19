
xVal(letterset, c(letters, LETTERS))

words <-
x_("/home/ryan/Desktop/war-and-peace.txt") $ xReadWords() $
xSelect(word := {
	all(xToChars(word) %in% letterset)
})

words $ xSortBy((w1 : w2) := nchar(w1, w2)) $ xTake(10)


