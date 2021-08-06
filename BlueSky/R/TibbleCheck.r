##01Feb2021
#dataframeName is a string and not the dataframeName object
#Checks if "tibble" class makes the object data.frame

BSkyCheckIfTibble <- function(dataframeName)
{

	if(eval( parse(text=paste('exists("',dataframeName,'", where=globalenv())',sep=''))))#exists
	{
		if(eval( parse(text=paste('is.data.frame(',dataframeName,')',sep='')))) ## is data.frame atleast
		{
			## get all the classes of the object
			storeDataFrameClasses <- eval (parse (text=paste ("class(", dataframeName, ")",sep="", collapse="") ))

			if ( "tbl_df" %in% storeDataFrameClasses) ## does tbl_df exists in the list of all class
			{
			eval(parse (text=paste ( ".GlobalEnv$" , dataframeName, "<-" ,"as.data.frame(", dataframeName, ")", sep="", collapse ="")))
			}

		}
	}

}

