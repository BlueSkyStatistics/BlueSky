returnFactorNamesOfFactorVars <- function (datasetname)
{

    eval(parse(text = paste("names(", datasetname,   ")[sapply(", datasetname, ",is.factor)]")))


}