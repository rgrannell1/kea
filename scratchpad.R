
raw_cran_data <-"
MDSGUI				2014-04-09 07:04:25		ligges
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
cran_data <-
	x_(raw_cran_data) $ xToLines() $
	xMap(row := xExplode("\t+", row)) $
	xMap(as.list) $ xMap(row := {

		row[[2]] <- as.Date( row[[2]] )
		xAddKeys(c("package", "time", "maintainer"), row)
	})

x_(cran_data) $ xGroupBy(x. $ maintainer)
