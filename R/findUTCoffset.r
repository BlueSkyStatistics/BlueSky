# converts all POSIXct class columns to UTC
MakeAllDateColUTC <- function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("MakeAllDateColUTC: Error setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex, sep="")
	BSkyWarnMsg = paste("MakeAllDateColUTC: Warning setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
		#colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)		
		colcount <- eval(parse(text=paste('length(names(',datasetname,'))' )))		
		#Error: dataSetName and colname not found
		for( colIndex in 1:colcount)  ##loop through each column
		{
			#cat("\nUTC offset adjustment starting: ")
			#get class of the column
			colclass = eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])')))
			utcOffsetSecs=0
			isPOSIXct = FALSE
			#cat("\nCol class: ")
			#cat(colclass)
			#colclass can have multiple class names so we loop through to find the one class that we are interested in
			for( j in 1:length(colclass))
			{
				if(colclass[j]=="POSIXct")
					isPOSIXct=TRUE
			}
			
			#cat("\nPOSIXct = ")
			#cat(isPOSIXct)
			#add utcOffsetSecs to POSIXct col
			if(isPOSIXct)
			{
				#eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"tzone")="UTC" ')))
				coltzone = eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"tzone")')))
				if(coltzone=="")
					coltzone = Sys.timezone()
				utcOffsetSecs = GetUTCoffsetSeconds(coltzone)
			
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
				#cat("\nConverting: ")
				#cat(colIndex)
				#if datasetname format is "Dataset2"
				#eval(parse(text=paste(datasetname,'[',colIndex,'] <<- ',datasetname,'[',colIndex,'] +', utcOffsetSecs, sep='')))
				
				#if datasetname format is ".GlobalEnv$Dataset2"
				eval(parse(text=paste(datasetname,'[',colIndex,'] <- ',datasetname,'[',colIndex,'] +', utcOffsetSecs, sep='')))
				#cat("\nConverted")
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
			}
		}
			
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("MakeAllDateColUTC:  Can't set UTC offset. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("MakeAllDateColUTC:  Can't set UTC offset. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()	
}


# converts specified POSIXct class columns to UTC
ConvertColToUTC <- function(colNameOrIndex, dataSetNameOrIndex)
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("ConvertToUTC: Error setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("ConvertToUTC: Warning setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
			#cat("\nUTC offset adjustment starting: ")
			#All the code below is for ajusting Excel datetime (local) to UTC by adding offset
			#
			#get class of the column
			colclass = eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])')))
			utcOffsetSecs=0
			isPOSIXct = FALSE
			#cat("\nCol class: ")
			#cat(colclass)
			#colclass can have multiple class names so we loop through to find the one class that we are interested in
			for( j in 1:length(colclass))
			{
				if(colclass[j]=="POSIXct")
					isPOSIXct=TRUE
			}
			
			#cat("\nPOSIXct = ")
			#cat(isPOSIXct)
			#add utcOffsetSecs to POSIXct col
			if(isPOSIXct)
			{
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
				# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
			
				coltzone = eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"tzone")')))
				if(coltzone=="")
					coltzone = Sys.timezone()
				utcOffsetSecs = GetUTCoffsetSeconds(coltzone)
				
				#cat("\nConverting: ")
				#cat(colIndex)
				eval(parse(text=paste(datasetname,'[',colIndex,'] <<- ',datasetname,'[',colIndex,'] -', utcOffsetSecs, sep='')))
				#cat("\nConverted")
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
			}
		}
		else
		{
			# cat("\nError: Cannot set col UTC offset. Col not found\n")
			BSkyErrMsg =paste("ConvertToUTC: Cannot set col UTC offset. Col not found."," Col Name:", colNameOrIndex)
			warning("ConvertToUTC: Cannot set col UTC offset. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("ConvertToUTC:  Can't set UTC offset. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("ConvertToUTC:  Can't set UTC offset. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()	
}


# get one UTC offset of a column. The column is not modified. We just get offset
GetColUTCoffsetSecs <- function(colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("GetColUTCoffsetSecs: Error setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("GetColUTCoffsetSecs: Warning setting col UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	utcOffsetSecs=0
	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
			#get class of the column
			colclass = eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])')))
			utcOffsetSecs=0
			isPOSIXct = FALSE
			#cat("\nCol class: ")
			#cat(colclass)
			#colclass can have multiple class names so we loop through to find the one class that we are interested in
			for( j in 1:length(colclass))
			{
				if(colclass[j]=="POSIXct")
					isPOSIXct=TRUE
			}
			
			#cat("\nPOSIXct = ")
			#cat(isPOSIXct)
			#add utcOffsetSecs to POSIXct col
			if(isPOSIXct)
			{
			
				coltzone = eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"tzone")')))
				if(is.null(coltzone) || coltzone=="") #### put a message that we didn't find TZ so we are using system TZ.
					coltzone = Sys.timezone()
				utcOffsetSecs = GetUTCoffsetSeconds(coltzone)			
				#cat("\n UTC Offset Secs: ")
				#cat(utcOffsetSecs)
			}
		}
		else
		{
			# cat("\nError: Cannot set col UTC offset. Col not found\n")
			BSkyErrMsg =paste("GetColUTCoffsetSecs: Cannot set col UTC offset. Col not found."," Col Name:", colNameOrIndex)
			warning("GetColUTCoffsetSecs: Cannot set col UTC offset. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("GetColUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("GetColUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()	
		return(utcOffsetSecs)
}

# get UTC offset for a specified timezone
GetUTCoffsetSeconds <- function(timezn)
{
	#get currrent local time
	loctime = format(Sys.time(), tz=timezn)
	
	#get current UTC time
	utctime = format(Sys.time(), tz="GMT")
	
	#offset in hours
	utcoffset = as.POSIXct(loctime) - as.POSIXct(utctime)
	
	# offset in seconds
	#utcoffsetSec = utcoffset * 3600 
	
	utcoffsetSec = as.numeric(utcoffset, units="secs")
	
	return(utcoffsetSec)
}


# get UTC offset of a cell of a column. The column is not modified. We just get offset
GetColCellUTCoffsetSecs <- function(rowidx, colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("GetColCellUTCoffsetSecs: Error setting col cell UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("GetColCellUTCoffsetSecs: Warning setting col cell UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	utcOffsetSecs=0
	if(!is.null(datasetname))
	{		
	
		totalrows = eval(parse(text=paste('nrow(',datasetname,')' )))
		if(rowidx >= 0 && rowidx < totalrows)
		{
			#nothing here
		}
		else
		{
			return(1); # return 0 UTC offset seconds as rowidx was invalid
		}
		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
			#get class of the column
			colclass = eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])')))
			utcOffsetSecs=0
			isPOSIXct = FALSE
			#cat("\nCol class: ")
			#cat(colclass)
			#colclass can have multiple class names so we loop through to find the one class that we are interested in
			for( j in 1:length(colclass))
			{
				if(colclass[j]=="POSIXct")
					isPOSIXct=TRUE
			}
			
			#cat("\nPOSIXct = ")
			#cat(isPOSIXct)
			#add utcOffsetSecs to POSIXct col
			if(isPOSIXct)
			{
			
				colcellposixctdate = eval(parse(text=paste(datasetname,'[',rowidx,',',colIndex,']')))
				colcellTzone <- strftime(colcellposixctdate, format="%z")
				
				tznum = as.numeric(colcellTzone)
				tzhrs = as.integer( tznum / 100 )
				tzmin = tznum %% 100

				utcOffsetSecs = tzhrs*3600 + tzmin*60
				
			}
		}
		else
		{
			# cat("\nError: Cannot set col UTC offset. Col not found\n")
			BSkyErrMsg =paste("GetColCellUTCoffsetSecs: Cannot set col cell UTC offset. Col not found."," Col Name:", colNameOrIndex)
			warning("GetColCellUTCoffsetSecs: Cannot set col cell UTC offset. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("GetColCellUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("GetColCellUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()	
		return(utcOffsetSecs)
}


# get separate UTC offset of each cell of a column. The column is not modified. We just get offsets
GetWholeColUTCoffsetSecs <- function(colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("GetWholeColUTCoffsetSecs: Error setting col cell UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("GetWholeColUTCoffsetSecs: Warning setting col cell UTC offset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	utcOffsetSecs=0
	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
			#get class of the column
			colclass = eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])')))
			utcOffsetSecs=0
			isPOSIXct = FALSE
			#cat("\nCol class: ")
			#cat(colclass)
			#colclass can have multiple class names so we loop through to find the one class that we are interested in
			for( j in 1:length(colclass))
			{
				if(colclass[j]=="POSIXct")
					isPOSIXct=TRUE
			}
			
			#cat("\nPOSIXct = ")
			#cat(isPOSIXct)
			#add utcOffsetSecs to POSIXct col
			if(isPOSIXct)
			{
				res =eval(parse(text=paste('
				lapply(',datasetname,'[,',colIndex,'], function(x) {colcellTzone <- strftime(x, format="%z");	tznum = as.numeric(colcellTzone);tzhrs = as.integer( tznum / 100 );	tzmin = tznum %% 100;tzhrs*3600 + tzmin*60})',sep='')))
				
				utcOffsetSecs = unlist(res)
				
			}
		}
		else
		{
			# cat("\nError: Cannot set col UTC offset. Col not found\n")
			BSkyErrMsg =paste("GetWholeColUTCoffsetSecs: Cannot set col cell UTC offset. Col not found."," Col Name:", colNameOrIndex)
			warning("GetWholeColUTCoffsetSecs: Cannot set col cell UTC offset. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("GetWholeColUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("GetWholeColUTCoffsetSecs:  Can't set UTC offset. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()	
		return(utcOffsetSecs)
}
