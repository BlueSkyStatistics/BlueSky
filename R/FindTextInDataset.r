FindInDataset <- function(searchtext,colNames=NULL, ignorecase=TRUE, dataSetNameOrIndex)
{

BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("FindInDataset: Error in FindInDataset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("FindInDataset: Warning in FindInDataset : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
colnamespresent=FALSE
		if(length(colNames) > 0 )
			colnamespresent = TRUE
			if(!is.null(datasetname))
			{		
				allrowidxs <- integer(0)
				allcolidxs <- integer(0)
				eval(parse(text=paste('colcount <- ncol(',datasetname,')' )))
				eval(parse(text=paste('allcolnames <- names(',datasetname,')' )))
				df<- data.frame()
					if(colnamespresent) # if col name passed then only iterate through those
					{
						colcount = length(colNames)
						for(i in 1 : colcount)
						{
							idxs <- integer(0)
							eval(parse(text=paste ( 'idxs <- grep(searchtext, as.character(',datasetname,'$',colNames[i],'), ignore.case=ignorecase)', sep=''))) 
							print(idxs)
							if(length(idxs) > 0)
							{
								#allrowidxs <- c(allrowidxs, idxs)
								
								#find col index
								colidx =  match( colNames[i], allcolnames)
								rc = dim(df)[1] # find current row count
								
								for(j in 1 : length(idxs) )
								{
									rc = rc+1 # finding next empty index
									df[rc,1] <- idxs[j]
									df[rc,2] <- colidx
								}								
							}		
						}						
					}
					else # col name not passed then iterate through all cols
					{
						#df<- data.frame()
						for(i in 1 : colcount)
						{
							idxs <- integer(0)
							eval(parse(text=paste ( 'idxs <- grep(searchtext,as.character(',datasetname,'[[i]]), ignore.case=ignorecase)', sep=''))) 
							print(idxs)
							if(length(idxs) > 0)
							{
								#allrowidxs <- c(allrowidxs, idxs)
								
								rc = dim(df)[1] # find current row count
								
								for(j in 1 : length(idxs) )
								{
									rc = rc+1 # finding next empty index
									df[rc,1] <- idxs[j]
									df[rc,2] <- i
								}
							}	
						}							
					}

			}
			else
			{
				# cat("\nError: Cannot Recode. Dataset name or index not found\n")
				BSkyErrMsg =paste("FindInDataset:  Cannot Find. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("FindInDataset:  Cannot Find. Dataset name or index not found.")
			}
		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		

		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)	
	
	    	if(BSkyLocalErrorFound() == TRUE)
    	{

    	}
		#cat("\nWARNING:: top level FindInDataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{

			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level Reode function\n")
		invisible(df)
}

#FindInDataset('Dataset2', '82')