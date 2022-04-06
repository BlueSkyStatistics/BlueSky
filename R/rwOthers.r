BSkyReadWithRio <- function(dataFilename, datasetname, replace=FALSE, encoding=NULL) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)

	BSkyErrMsg = paste("BSkyReadWithRio: Error reading file : ", "DataSetName :", datasetname," ", "Filename  :", paste(dataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyReadWithRio: Warning reading file : ", "DataSetName :", datasetname," ", "Filename :", paste(dataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1

	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )

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

		# cat("\nBefore Try Catch\n")
		corecommand=c()
		#R command to open data file (SPSS)
		if(is.null(encoding))
		{
			corecommand = paste('rio::import(file=\'',dataFilename,'\')',sep='')
		}
		else{
			corecommand = paste('rio::import(file=\'',dataFilename,'\', encoding=\'',encoding,'\')', sep='')
		}
		opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( ',corecommand,')', sep='')
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		#trying to open the datafile
		tryCatch({
		
				withCallingHandlers({
					eval(parse(text = opendatafilecmd))
				}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
				}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
		{
			success = 0
			## maybe return 0 for success
			cat("\nSuccessfully opened using:\n") 
			print(corecommand) #no need to print this
		}
		else ## Failure
		{
			print(paste('Current system encoding: cp',l10n_info()$codepage,sep=''))
			cat("\nError opening file:\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}	

		if(success==0)
		{
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
	}		
	else
	{
		BSkyErrMsg =paste("BSkyReadWithRio: Dataset with the same name already on the global list ."," Dataset Name:", datasetname)
		warning("BSkyReadWithRio: Dataset with the same name already on the global list ")
	}			

	BSkyFunctionWrapUp()
	return(success)
}



BSkyWriteWithRio <- function(dataFilename,dataSetNameOrIndex) ##  index of dataset and file
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyWriteWithRio: Error writing file : ", "DataSetName :", dataSetNameOrIndex," ", "Filename  :", paste(dataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyWriteWithRio: Warning writing file : ", "DataSetName :", dataSetNameOrIndex," ", "Filename :", paste(dataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#cat("\nDS obj:", datasetname," ::\n")
	if(!is.null(datasetname))
	{		
		corecommand = paste('rio::export(x=',datasetname,', file = "',dataFilename,'")',sep='')
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())		
		#trying to save the datafile
		tryCatch({
		
				withCallingHandlers({
					eval(parse(text=corecommand))
				}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
				}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
		{
			success = 0
			## maybe return 0 for success
			# cat("\nSuccessfully saved\n") 
			# print(corecommand) #no need to print this
		}
		else ## Failure
		{
			cat("\nError saving file:\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}		
	}
	else
	{
		BSkyErrMsg =paste("BSkyWriteWithRio:  Cannot write. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyWriteWithRio:  Cannot write. Dataset name or index not found.")
	}		
	BSkyFunctionWrapUp()	
	return(success)

}


#convert file format (sav to csv) without opening it, using rio
BSkyConvertFileFormat <- function(sourceFile, targetFile)
{
	BSkyFunctionInit()
	success=-1
	
	#rio::convert("D:/Datasets/rio-test-datasets/mydataset3.xml", "D:/Datasets/rio-test-datasets/mydataset3.sav")
	corecommand = paste('rio::convert(in_file=',sourceFile,', out_file = "',targetFile,'")',sep='')
	eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())		
	#trying to save the datafile
	tryCatch({
	
			withCallingHandlers({
				eval(parse(text=corecommand))
			}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
			}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
	
	if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
	{
		success = 0
		## maybe return 0 for success
		# cat("\nSuccessfully saved\n") 
		# print(corecommand) #no need to print this
	}
	else ## Failure
	{
		cat("\nError saving file:\n") 
		# cat("\n\nCommand executed:\n")
		print(corecommand)
		## gracefully report error to the app layer about the issue so it does not keep waiting. 
		## maybe return -1 for failure
		success = -1;
	}		
		
	BSkyFunctionWrapUp()	
	return(success)

}
