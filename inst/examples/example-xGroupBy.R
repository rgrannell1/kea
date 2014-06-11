
# 1.
#
# Data gathered using Hadley Wickham's `cranatic` repository.
#
# Group the following table by package maintainer.

raw_cran_data <-
"MDSGUI             07:04:25    ligges
tripack             06:55:24    ligges
zipfR               08:08:00    hornik
PolynomF            07:08:57    hornik
FRBData             06:59:42    hornik
gldist              07:38:15    hornik
oce                 07:48:04    hornik
sltl                08:00:33    ripley
DistributionUtils   06:58:06    hornik
phylosim            07:50:53    ripley
mclust              05:43:41    ripley
RhpcBLASctl         07:14:51    ripley
spBayes             08:00:52    ripley
ISLR                07:02:39    ligges
RecordLinkage       07:14:25    hornik
ProjectTemplate     07:09:12    ripley
hydroApps           07:39:38    hornik
shiny               05:32:22    hornik
pequod              07:50:28    ripley
ICEbox              07:02:26    ripley
"

# parse the table, and add rownames.
cran_data <-
	x_(raw_cran_data) $ xToLines() $
	xMap(row := xExplode("[ ]+", row)) $
	xMap(as.list) $
	xMap( xAddKeys(c("package", "time", "maintainer")) )

x_(cran_data) $ x_GroupBy(x. $ maintainer)

# list(
#     list("ligges",
#         list(
#             list(package = "MDSGUI    ",        time = "07:04:25", maintainer = "ligges"),
#             list(package = "tripack",           time = "06:55:24", maintainer = "ligges"),
#             list(package = "ISLR",              time = "07:02:39", maintainer = "ligges")
#         )
#     ),
#     list("hornik",
#         list(
#             list(package = "zipfR",             time = "08:08:00", maintainer = "hornik"),
#             list(package = "PolynomF",          time = "07:08:57", maintainer = "hornik"),
#             list(package = "FRBData",           time = "06:59:42", maintainer = "hornik"),
#             list(package = "gldist",            time = "07:38:15", maintainer = "hornik"),
#             list(package = "oce",               time = "07:48:04", maintainer = "hornik"),
#             list(package = "DistributionUtils", time = "06:58:06", maintainer = "hornik"),
#             list(package = "RecordLinkage",     time = "07:14:25", maintainer = "hornik"),
#             list(package = "hydroApps",         time = "07:39:38", maintainer = "hornik"),
#             list(package = "shiny",             time = "05:32:22", maintainer = "hornik")
#         )
#     ),
#     list("ripley",
#         list(
#             list(package = "sltl",              time = "08:00:33", maintainer = "ripley"),
#             list(package = "phylosim",          time = "07:50:53", maintainer = "ripley"),
#             list(package = "mclust",            time = "05:43:41", maintainer = "ripley"),
#             list(package = "RhpcBLASctl",       time = "07:14:51", maintainer = "ripley"),
#             list(package = "spBayes",           time = "08:00:52", maintainer = "ripley"),
#             list(package = "ProjectTemplate",   time = "07:09:12", maintainer = "ripley"),
#             list(package = "pequod",            time = "07:50:28", maintainer = "ripley"),
#             list(package = "ICEbox",            time = "07:02:26", maintainer = "ripley")
#         )
#     )
# )
