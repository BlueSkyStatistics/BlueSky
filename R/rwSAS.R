## Following function not at all ready
BSkyLoadSASinDataFrame <- function(SASfilename, datasetname, replace=FALSE, encoding=NULL)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("BSkyLoadSASinDataFrame: Error reading SAS file : ", "DataSetName :", datasetname," ", "SAS filename  :", paste(SASfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyLoadSASinDataFrame: Warning reading SAS file : ", "DataSetName :", datasetname," ", "SAS filename :", paste(SASfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	##loading the dbf file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of SAS:",curidx)
	if((replace == TRUE) || (replace == FALSE && curidx == -1))
	{
		# if i>0 then datasetname already exists && you want to replace it.
		if(replace == TRUE && curidx > 0)
		{
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
			corecommand = paste('haven::read_sas(data_file=\'',SASfilename,'\')',sep='')
			# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_sas(file=\'',SASfilename,'\'))',sep='')
		}
		else{
			corecommand = paste('read_sas(data_file=\'',SASfilename,'\', encoding=\'',encoding,'\')', sep='')
			# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_sas(file=\'',SASfilename,'\', encoding=\'',encoding,'\'))',sep='')
		}
		opendatafilecmd = paste('x <- as.data.frame( ',corecommand,')', sep='')
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		#trying to open the datafile
		tryCatch({
		
				withCallingHandlers({
					eval(parse(text = opendatafilecmd))
					eval(parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )', sep='' )))
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
			cat("\nError opening file:\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}	

		#df <- read_sas("D:/BlueSky Statistics/Misc/Data_Files/SAS/ctcodes-procedures.sas7bdat")
		##following 2 lines were in use before(11Nov2021) above tryCatch
		# eval( parse(text=paste('x <- as.data.frame( read_sas(\'',SASfilename,'\'))', sep=''  ))) # use with lapply
		# eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )' )))
		
		if(success==0)
		{
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list
			# names(uadatasets$lst)[length(uadatasets$lst)]<-datasetname
			uadatasets$name <- c(uadatasets$name, datasetname)
			#cat("\nLoaded SAS dataset :", datasetname)					
			
			# if(replace == FALSE)
			# {						
				# #for old code compatibility put same list in 'name' also
				# uadatasets$name <- c(uadatasets$name, datasetname)
			# }	
						
			# DataSetIndex <- UAgetIndexOfDataSet(datasetname)


			#Creating extra attributes at column level
			UAcreateExtraAttributes(datasetname, "SAS")		
		}		
	}
	else
	{
		BSkyErrMsg =paste("BSkyLoadSASinDataFrame: Dataset with the same name already on the global list."," Dataset Name:", datasetname)
		warning("BSkyLoadSASinDataFrame: Dataset with the same name already on the global list ")
	}				
	BSkyFunctionWrapUp()
	return(success)

}


BSkyWriteSas <- function(sasFilename,dataSetNameOrIndex) ##  index of dataset and file
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyWriteSas: Error writing SAS file : ", "DataSetName :", dataSetNameOrIndex," ", "SAS Filename  :", paste(sasFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyWriteSas: Warning writing SAS file : ", "DataSetName :", dataSetNameOrIndex," ", "SAS Filename :", paste(sasFilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#cat("\nDS obj:", datasetname," ::\n")
	if(!is.null(datasetname))
	{		
		corecommand = paste('haven::write_sas(',datasetname,', path = "',sasFilename,'")', sep='')
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())		
		#trying to save the datafile
		tryCatch({
		
				withCallingHandlers({
					datasetname <- ExtractDatasetNameFromGlobal(datasetname)
					eval(parse(text=paste('tmpsasx <- as.data.frame( lapply (',datasetname,', function(y) {if(class(y) == "factor") y = as.character(y) else y} ) )' )))
					eval(parse(text=paste('haven::write_sas(tmpsasx, path = "',sasFilename,'")',sep='')))
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
		# cat("\nError: Cannot write SAS. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkyWriteSas:  Cannot write SAS. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyWriteSas:  Cannot write SAS. Dataset name or index not found.")
	}		
	BSkyFunctionWrapUp()			
	return(success)

}
