
# 1.
#
# Data gathered using Hadley Wickham's `cranatic` repository.
#
# Group the following table by package maintainer.

raw_cran_data <-
"MDSGUI				2014-04-09 07:04:25		ligges
tripack				2014-04-09 06:55:24		ligges
zipfR				2014-04-09 08:08:00		hornik
PolynomF			2014-04-09 07:08:57		hornik
FRBData				2014-04-09 06:59:42		hornik
gldist				2014-04-09 07:38:15		hornik
oce					2014-04-09 07:48:04		hornik
sltl				2014-04-09 08:00:33		ripley
DistributionUtils	2014-04-09 06:58:06		hornik
phylosim			2014-04-09 07:50:53		ripley
mclust				2014-04-09 05:43:41		ripley
RhpcBLASctl			2014-04-09 07:14:51		ripley
spBayes				2014-04-09 08:00:52		ripley
ISLR				2014-04-09 07:02:39		ligges
RecordLinkage		2014-04-09 07:14:25		hornik
ProjectTemplate		2014-04-09 07:09:12		ripley
hydroApps			2014-04-09 07:39:38		hornik
shiny				2014-04-09 05:32:22		hornik
pequod				2014-04-09 07:50:28		ripley
ICEbox				2014-04-09 07:02:26		ripley
"

# parse the table, and add rownames.
cran_data <-
	x_(raw_cran_data) $ xToLines() $
	xMap(row := xExplode("[	]+", row)) $
	xMap(as.list) $ xMap(row := {
		xAddKeys(c("package", "time", "maintainer"), row)
	})

x_(cran_data) $ x_GroupBy(x. $ maintainer)

# list(
#     list("ligges",
#         list(
#             list(package = "MDSGUI    ",        time = "2014-04-09 07:04:25", maintainer = "ligges"),
#             list(package = "tripack",           time = "2014-04-09 06:55:24", maintainer = "ligges"),
#             list(package = "ISLR",              time = "2014-04-09 07:02:39", maintainer = "ligges")
#         )
#     ),
#     list("hornik",
#         list(
#             list(package = "zipfR",             time = "2014-04-09 08:08:00", maintainer = "hornik"),
#             list(package = "PolynomF",          time = "2014-04-09 07:08:57", maintainer = "hornik"),
#             list(package = "FRBData",           time = "2014-04-09 06:59:42", maintainer = "hornik"),
#             list(package = "gldist",            time = "2014-04-09 07:38:15", maintainer = "hornik"),
#             list(package = "oce",               time = "2014-04-09 07:48:04", maintainer = "hornik"),
#             list(package = "DistributionUtils", time = "2014-04-09 06:58:06", maintainer = "hornik"),
#             list(package = "RecordLinkage",     time = "2014-04-09 07:14:25", maintainer = "hornik"),
#             list(package = "hydroApps",         time = "2014-04-09 07:39:38", maintainer = "hornik"),
#             list(package = "shiny",             time = "2014-04-09 05:32:22", maintainer = "hornik")
#         )
#     ),
#     list("ripley",
#         list(
#             list(package = "sltl",              time = "2014-04-09 08:00:33", maintainer = "ripley"),
#             list(package = "phylosim",          time = "2014-04-09 07:50:53", maintainer = "ripley"),
#             list(package = "mclust",            time = "2014-04-09 05:43:41", maintainer = "ripley"),
#             list(package = "RhpcBLASctl",       time = "2014-04-09 07:14:51", maintainer = "ripley"),
#             list(package = "spBayes",           time = "2014-04-09 08:00:52", maintainer = "ripley"),
#             list(package = "ProjectTemplate",   time = "2014-04-09 07:09:12", maintainer = "ripley"),
#             list(package = "pequod",            time = "2014-04-09 07:50:28", maintainer = "ripley"),
#             list(package = "ICEbox",            time = "2014-04-09 07:02:26", maintainer = "ripley")
#         )
#     )
# )
