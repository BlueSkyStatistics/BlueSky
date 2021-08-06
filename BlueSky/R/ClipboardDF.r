CreateDFfromClipboard <- function (dfname, sep = "\t", header=TRUE)
{
eval(parse(text=paste(dfname,'<<-read.table(file = "clipboard", sep = sep, header=header)',sep='')))
invisible()
}

## Supported on Windows, OS X, and X11 clipboards
CreateDFfromAnyOSClipboard <- function (dfname, sep = "\t", header=TRUE, strAdFac=FALSE, naStr=c("NA", ""), trimWhite=TRUE)
{
require(clipr)
eval(parse(text=paste(dfname,'<<-read_clip_tbl()(file = "clipboard", sep = sep, header=header, stringsAsFactors=strAdFac, na.strings=naStr, strip.white=trimWhite)',sep='')))
invisible()
} 

BSkyPasteData <- function(rowidx, colidx, dataSetNameOrIndex)
{
}