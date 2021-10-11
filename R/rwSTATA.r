## copied from rwObj and then modified for stata. So comments may not make sense in all places in the code in this file.

BSkyReadStata <- function(stataFilename, datasetname, replace=FALSE) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)

	BSkyErrMsg = paste("BSkyReadStata: Error reading Stata file : ", "DataSetName :", datasetname," ", "Stata Filename  :", paste(stataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyReadStata: Warning reading Stata file : ", "DataSetName :", datasetname," ", "Stata Filename :", paste(stataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	##loading the dbf file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of RObj:",curidx)
	if((replace == TRUE) || (replace == FALSE && curidx == -1)){
		# if i>0 then datasetname already exists && you want to replace it.
		if(replace == TRUE && curidx > 0){
			# Delete the existing dataset from the global list first. 
			# if dataset already exists, its index is in i.
			# uadatasets$lst[i]<-NULL
			#UAcloseDataset(datasetname)
			BSkyCloseForReloading(datasetname)
			#cat("DS Closed")						
		}

		eval( parse(text=paste( '.GlobalEnv$',datasetname,' <- as.data.frame(read_dta(file = "',stataFilename,'"))' , sep=''   )))
		colcount = eval(parse(text=paste('ncol(',datasetname,')')))
		for(i in 1:colcount)
		{
			coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
			colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))

			if(colclass[1] == "haven_labelled")##"haven_labelled" "vctrs_vctr"     "double"
			{
				eval(parse(text=paste('.GlobalEnv$',datasetname,'$',coluname,'<- as_factor(',datasetname,'$',coluname,')',sep='' )))
			}
		}
		uadatasets$name <- c(uadatasets$name, datasetname)

		#Creating extra attributes at column level
		UAcreateExtraAttributes(datasetname, "DTA")
		
	}		
	else
	{
		BSkyErrMsg =paste("BSkyReadStata: Dataset with the same name already on the global list ."," Dataset Name:", datasetname)
		warning("BSkyReadStata: Dataset with the same name already on the global list ")
	}			

	BSkyFunctionWrapUp()
}



BSkyWriteStata <- function(stataFilename,dataSetNameOrIndex) ##  index of dataset and file
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkywriteStata: Error writing Stata file : ", "DataSetName :", dataSetNameOrIndex," ", "Stata Filename  :", paste(stataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkywriteStata: Warning writing Stata file : ", "DataSetName :", dataSetNameOrIndex," ", "Stata Filename :", paste(stataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#cat("\nDS obj:", datasetname," ::\n")
	if(!is.null(datasetname))
	{		
		#datasetname <- ExtractDatasetNameFromGlobal(datasetname)
	
		eval(parse(text=paste('write_dta(',datasetname,', path = "',stataFilename,'")',sep='')))
	}
	else
	{
		# cat("\nError: Cannot write Stata. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkywriteStata:  Cannot write Stata. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkywriteStata:  Cannot write Stata. Dataset name or index not found.")
	}		
	BSkyFunctionWrapUp()			

}
