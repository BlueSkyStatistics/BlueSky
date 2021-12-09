# Parameters description
# datasetname : string name of the dataset
# datasetobj  : dataset object name (same as datasetname abv.) that holds the actual dataset
# RPkgName    : The R package from which the datasetname (datasetobj) has to be loaded
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Load dataset from a package
#'
#' @description 
#'
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#'
#' @return
#'
#' @examples

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