
nm <- names(res)

nm_abd <- nm[grepl("^Abd ", nm)]
nm_abd

nm_bio <- nm[grepl("^Bio ", nm)]
nm_bio

res[,c("Id", nm_abd)]
res[,c("Id", nm_bio)]

names(attributes(res))
spec <- attr(res, "spectrum")
spec
class(spec)
names(spec)
spec[["MTLG.2005-05-24.H1"]]
