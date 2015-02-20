
#
# a subset of the 1999 Clinton impeachment vote.
# each row of the data represents a candidate;
# the columns the party, perjury vote and obstruction vote respectively.

raw_clinton_vote <- "
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	R	NotGuilty	NotGuilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	R	NotGuilty	NotGuilty
	D	NotGuilty	NotGuilty
	R	Guilty		Guilty
	R	Guilty		Guilty
	D	NotGuilty	NotGuilty"

clinton_vote <-
	x_(raw_clinton_vote) $ xToLines() $ xMap(xToWords) $
	xZip() $ xAddKeys(c('party', 'perjury', 'obstruction'))

# 1.
# Tabulate the overall results for the perjury vote

clinton_vote $ xAtKey('perjury') $ x_Tabulate()

# list( list("Guilty", 12), list("NotGuilty", 14) )

# 2.
# Tabulate the overall results for the obstruction vote

clinton_vote $ xAtKey('obstruction') $ x_Tabulate()

# list( list("Guilty", 12), list("NotGuilty", 14) )

# 3.
# An advanced example;
# get the overall vote within each party.

clinton_vote $ xZip () $ xAmassBy(xFirstOf) $ # -- group by party
xMap(party := {
	# get the perjury and obstruction votes by party.

	list(
		perjury =
			x_(party) $ xMap(xSecondOf) $ xTabulate() $ x_ZipKeys(),
		obstruction =
			x_(party) $ xMap(xFirstOf)  $ xTabulate() $ x_ZipKeys()
	)

}) $
x_AddKeys(c('Republican', 'Democrat'))


# list(
#     Republican =
#         list(
#             perjury =
#                 list(Guilty = 12, NotGuilty = 2),
#             obstruction =
#                 list(Guilty = 12, NotGuilty = 2),
#     Democrat =
#         list(
#             perjury =
#                 list(NotGuilty = 12),
#             obstruction =
#                 list(NotGuilty = 12)
#         )
#     )
# )
