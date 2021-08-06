
naCheck <- function (weights, data)
{
	bool =eval (parse( text =paste ("any(is.na(", data, "$", weights, "))", collapse="",sep="")))
	return(bool)
}

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
		errorMsg1 =paste("ERROR: There are missing values (NAs) in the weighted variable ", weights, "\n please corect this and rerun. See Data->Missing Values for options for handling missing values", weights, sep="", collapse="")
		print(errorMsg1)
		return(FALSE)
	}
}