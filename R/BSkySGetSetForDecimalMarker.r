#Global Setting for delim character marker for csv file read and cut/paste data
BSkySetDelimMarker <- function(delimMarker = '') 
{	
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkyDelimMarker = delimMarker[1]
	}
	
	# Just a dummy return 
	return(invisible(delimMarker[1]))
}

# 07/08/24
#Get the Global Setting for delim character marker for csv file read and cut/paste data
BSkyGetDelimMarker <- function()
{
	delimMarker = ''
	
	if(exists("uadatasets.sk") && exists("BSkyDelimMarker", env=uadatasets.sk))
	{
		delimMarker = uadatasets.sk$BSkyDelimMarker[1]
	}
	
	#return the actual delim marker character that is currently set (from the triple dot setting options UI)
	return(invisible(delimMarker))
}

# 07/08/24
#Global Setting for decimal character marker for csv file read and cut/paste data
BSkySetDecimalMarker <- function(decimalMarker = '') 
{	
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkyDecimalMarker = decimalMarker[1]
	}
	
	# Just a dummy return 
	return(invisible(decimalMarker[1]))
}

# 07/08/24
#Get the Global Setting for decimal character marker for csv file read and cut/paste data
BSkyGetDecimalMarker <- function()
{
	decimalMarker = ''
	
	if(exists("uadatasets.sk") && exists("BSkyDecimalMarker", env=uadatasets.sk))
	{
		decimalMarker = uadatasets.sk$BSkyDecimalMarker[1]
	}
	
	#return the actual decimal marker character that is currently set (from the triple dot setting options UI)
	return(invisible(decimalMarker))
}

# 07/08/24
#Global Setting for display character marker for csv file read and cut/paste data
BSkySetDisplayMarker <- function(displayMarker = '') 
{	
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkyDisplayMarker = displayMarker[1]
	}
	
	# Just a dummy return 
	return(invisible(displayMarker[1]))
}

# 07/08/24
#Get the Global Setting for display character marker for csv file read and cut/paste data
BSkyGetDisplayMarker <- function()
{
	displayMarker = ''
	
	if(exists("uadatasets.sk") && exists("BSkyDisplayMarker", env=uadatasets.sk))
	{
		decimalMarker = uadatasets.sk$BSkyDisplayMarker[1]
	}
	
	#return the actual display marker character that is currently set (from the triple dot setting options UI)
	return(invisible(displayMarker))
}
