######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAreadDBF
# UAwriteDBF
#################################################################################################

###################################################################################################################
#Function: UAreadDBF(dbffilename,  datasetname, replace=FALSE)							
#              																			
#Parameters:  
#		dbffilename			- full path filename of DBF file on disk
#		datasetname     	- new refrence name for later use   
#		replace				- TRUE if you want to overwrite exisiting dataset in UA memory space    																
#																						
#Description:	
#		Reads dbf file from disk and loads in UA memory space. You can refer this dataset later 
#		with provided datasetname
#																						
#Example: UAreadDBF("C:/Data/test.dbf",  "mydbf", replace=FALSE)																			
###################################################################################################################
UAreadDBF <- function(dbffilename,  datasetname, replace=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("UAreadDBF: Error reading DBF : ", "DataSetName :", datasetname," ", "DBF filename  :", paste(dbffilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAreadDBF: Warning reading DBF : ", "DataSetName :", datasetname," ", "DBF filename :", paste(dbffilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=0
	#library(foreign)

	##loading the dbf file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of DBF:",curidx)
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
		corecommand = paste('read.dbf(file=\'',dbffilename,'\')',sep='')
		# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read.dbf(file=\'',stataFilename,'\'))',sep='')

		opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- ',corecommand, sep='')
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
			## maybe return 0 for success
			# cat("\nSuccessfully opened\n") 
			# print(corecommand) #no need to print this
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

		
		
		# Now add new dataset.
		# uadatasets$lst <- c(uadatasets$lst, list(read.dbf(dbffilename)))
		
		##following line was in use before(11Nov2021) above tryCatch
		# eval( parse(text=paste('.GlobalEnv$',datasetname,' <- read.dbf(\'',dbffilename,'\')', sep='' )))
		
		if(success==0)
		{
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list
			# names(uadatasets$lst)[length(uadatasets$lst)]<-datasetname
			uadatasets$name <- c(uadatasets$name, datasetname)
			cat("\nLoaded DBF dataset :", datasetname)				
			
			# if(replace == FALSE)
			# {						
				# #for old code compatibility put same list in 'name' also
				# uadatasets$name <- c(uadatasets$name, datasetname)
			# }					

			#Creating extra attributes at column level
			UAcreateExtraAttributes(datasetname, "DBF")
		}
	}
	else
	{
		BSkyErrMsg =paste("UAreadDBF: Dataset with the same name already on the global list"," Dataset Name:", datasetname)
		warning("UAreadDBF: Dataset with the same name already on the global list ")
	}					
	BSkyFunctionWrapUp()
	return(success)
}

###################################################################################################################
#Function: UAwriteDBF(dbffilename, dataSetNameOrIndex, fact2char)								
#              																			
#Parameters:  
#		dbffilename				- full path filename of new DBF file to which particular dataset element 
#								  of uadatasets$lst is to be written
#		dataSetNameOrIndex     	- Dataset Name or Index (of uadatasets$lst array) which is to be 
#								  written to disk file as new DBF file. 
#		fact3char				- Factor col are written in character if TRUE. Else they will contain integer 
#																						
#Description:	
#		Write UA memory space dataset to a dbf file format
#																						
#Example: UAwriteDBF("C:/Data/new.dbf", "dietstudy")																				
###################################################################################################################
UAwriteDBF <- function(dbffilename, dataSetNameOrIndex, fact2char = TRUE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAwriteDBF: Error writing DBF : ", "DataSetName :", dataSetNameOrIndex," ", "DBF filename  :", paste(dbffilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAwriteDBF: Warning writing DBF : ", "DataSetName :", dataSetNameOrIndex," ", "DBF filename :", paste(dbffilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=0
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!is.null(datasetname))
	{		
		corecommand = paste('write.dbf(',datasetname,', dbffilename, factor2char = fact2char)')
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
		##following command was in use before adding tryCatch above
		# eval(parse(text=paste('write.dbf(',datasetname,', dbffilename, factor2char = fact2char)')))
	}
	else
	{
		# cat("Error: Cannot get Split. Dataset name or index not found")
		BSkyErrMsg =paste("UAwriteDBF: Cannot get Split. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("UAwriteDBF: Cannot get Split. Dataset name or index not found.")
	}			
	BSkyFunctionWrapUp()	
	return(success)
}

