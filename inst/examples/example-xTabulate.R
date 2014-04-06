
# a subset of the 1999 impeachment vote

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

# 2. # Tabulate the overall results for the obstruction vote

clinton_vote $ xAtKey('obstruction') $ x_Tabulate()

# list( list("Guilty", 12), list("NotGuilty", 14) )
