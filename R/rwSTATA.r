## copied from rwObj and then modified for stata. So comments may not make sense in all places in the code in this file.

BSkyReadStata <- function(stataFilename, datasetname, replace=FALSE, encoding=NULL) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)

	BSkyErrMsg = paste("BSkyReadStata: Error reading Stata file : ", "DataSetName :", datasetname," ", "Stata Filename  :", paste(stataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyReadStata: Warning reading Stata file : ", "DataSetName :", datasetname," ", "Stata Filename :", paste(stataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
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

		# cat("\nBefore Try Catch\n")
		corecommand=c()
		#R command to open data file (SPSS)
		if(is.null(encoding))
		{
			corecommand = paste('haven::read_dta(file=\'',stataFilename,'\')',sep='')
			# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_dta(file=\'',stataFilename,'\'))',sep='')
		}
		else{
			corecommand = paste('haven::read_dta(file=\'',stataFilename,'\', encoding=\'',encoding,'\')', sep='')
			# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_dta(file=\'',stataFilename,'\', encoding=\'',encoding,'\'))',sep='')
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

		##following line was in use before(11Nov2021) above tryCatch
		# eval( parse(text=paste( '.GlobalEnv$',datasetname,' <- as.data.frame(read_dta(file = "',stataFilename,'"))' , sep=''   )))
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
		BSkyErrMsg =paste("BSkyReadStata: Dataset with the same name already on the global list ."," Dataset Name:", datasetname)
		warning("BSkyReadStata: Dataset with the same name already on the global list ")
	}			

	BSkyFunctionWrapUp()
	return(success)
}



BSkyWriteStata <- function(stataFilename,dataSetNameOrIndex) ##  index of dataset and file
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkywriteStata: Error writing Stata file : ", "DataSetName :", dataSetNameOrIndex," ", "Stata Filename  :", paste(stataFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkywriteStata: Warning writing Stata file : ", "DataSetName :", dataSetNameOrIndex," ", "Stata Filename :", paste(stataFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#cat("\nDS obj:", datasetname," ::\n")
	if(!is.null(datasetname))
	{		
		corecommand = paste('haven::write_dta(',datasetname,', path = "',stataFilename,'")',sep='')
		#reset global error-warning flag
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
		
		#datasetname <- ExtractDatasetNameFromGlobal(datasetname)
		# following command was in use before adding tryCatch above
		# eval(parse(text=paste('write_dta(',datasetname,', path = "',stataFilename,'")',sep='')))
	}
	else
	{
		# cat("\nError: Cannot write Stata. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkywriteStata:  Cannot write Stata. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkywriteStata:  Cannot write Stata. Dataset name or index not found.")
	}		
	BSkyFunctionWrapUp()	
	return(success)

}
