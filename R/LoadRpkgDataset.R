# Parameters description
# datasetname : string name of the dataset
# datasetobj  : dataset object name (same as datasetname abv.) that holds the actual dataset
# RPkgName    : The R package from which the datasetname (datasetobj) has to be loaded
BSkyLoadRpkgDataset <- function(datasetname, datasetobj, RPkgName)
{
	eval(parse(text = paste( "TmP=data('" , datasetname , "', package='" , RPkgName , "')" , sep = ''))) 

	dsclass = eval(parse(text=paste('class(',datasetobj,')',sep='')))
	if(!('data.frame' %in% dsclass))
	{
		eval(parse(text = paste(datasetobj , "<<- as.data.frame( ",datasetobj,")", sep = '')))
	}

	#return; #prints .Primitive("return")
	invisible() #print NULL
}