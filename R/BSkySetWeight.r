
naCheck <- function (weights, data)
{
	bool =eval (parse( text =paste ("any(is.na(", data, "$", weights, "))", collapse="",sep="")))
	return(bool)
}

### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Expand weights
#'
#' @description Creates a new dataset with rows expanded as per weights. Expands (replicates) the rows of a data.frame  by a value contained in one of the columns in the source data.frame or data.table. BSkySetWeights() calls expandRows() from the package splitstackshape.
#'
#' @param weights The dataset variable that contains the weights.
#' @param data The input data.frame or data.table.
#' @param newdata The new dataset where the rows are replicated for the weights specified.
#'
#' @return expanded dataset according to the weights
#'
#' @examples
BSkySetWeight <- function( weights, data, newdata)
{
	BSkybool = !naCheck(weights,data)
	if (BSkybool )
	{
		eval( parse(text=paste(".GlobalEnv$",newdata, "<- expandRows(",data,",", deparse(weights),")" , sep="", collapse="") ) )
		return(TRUE)
	}
	else
	{
		errorMsg1 =paste("ERROR: There are missing values (NAs) in the weighted variable ", weights, "\n please correct this and rerun. See Variables->Missing Values for options for handling missing values", sep="", collapse="")
		cat(errorMsg1)
		return(FALSE)
	}
}