
# 1. How many ways can you get one or more royals from a random hand of two cards?
#    Determine by exhaustive search.

cards <-x_(xProdSetOf_(
	c('spades', 'hearts', 'diamonds', 'clubs'),
	c('ace', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'Jack', 'Queen', 'King') ))

pairs <- cards $ xChoose(2)

royals <- c('Jack', 'Queen', 'King')

pairs $ xPoll( xUnspread((card1 : card2) := {

	rank1 <- xSecondOf(card1)
	rank2 <- xSecondOf(card2)

	xIsMember(rank1, royals) || xIsMember(rank2, royals)
}) )

# 546 different ways
