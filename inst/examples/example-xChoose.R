
# 1. How many ways can you get one or more royals from a random hand of two cards?
#    Determine by exhaustive search.

cards_ <-x__(
	'ace', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'Jack', 'Queen', 'King') $ xRepeat(4)

is_royal <- xIsIn(coll = c('Jack', 'Queen', 'King'))

cards_ $ xChoose(2) $ xPoll(xAnyOf(is_royal))

# 546 different ways
