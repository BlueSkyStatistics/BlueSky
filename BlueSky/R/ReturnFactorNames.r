returnFactorNamesOfFactorVars <- function (datasetname, cross=FALSE)
{

    if (!cross)
	{
	eval(parse(text = paste("names(", datasetname,   ")[sapply(", datasetname, ",is.factor)]")))
	}
	else
	{
	c( "",eval(parse(text = paste("names(", datasetname,   ")[sapply(", datasetname, ",is.factor)]"))))
	}


}