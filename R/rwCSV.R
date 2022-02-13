######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAreadCSV
# UAwriteCSV
###################################################################################################################
#Function: UAreadCSV(csvfilename, datasetName, Header=TRUE, replace=FALSE)						
#              																			
#Parameters:  
#		csvfilename			- full path filename of CSV file which is to be read from disk
#		datasetName     	- new refrence name for later use  
#		Header				- TRUE if first row in .csv file is treated as column names 
#		replace 			- TRUE if you want to overwrite exisiting dataset in UA memory space     																
#
#Description:	
#		Reads csv file from disk and loads in UA memory space. You can refer this dataset later 
#		with provided datasetname
#
#Example: UAreadCSV("C:/Data/test.csv", "mycsv", Header=TRUE, replace=FALSE)
###################################################################################################################
UAreadCSV <- function(csvfilename, datasetname, Header=TRUE, replace=FALSE,character.to.factor=FALSE, sepCh=',', deciCh='.')
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("UAreadCSV: Error reading CSV file : ", "DataSetName :", datasetname," ", "CSV filename  :", paste(csvfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAreadCSV: Warning reading CSV file : ", "DataSetName :", datasetname," ", "CSV filename  :", paste(csvfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success= -1
	logflag=FALSE
	
	##for testing UI app when exception occurs in R
	# if(success==0) 
	# {
		# e <- simpleError("Error in opening CSV file.")
		# stop(e)
	# }
	
	##for testing UI app when file could not be opened in R
	# return(-1) #i.e. failed to open file
	
	##loading the csv file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already be present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of CSV:",curidx)
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
		#ptm= proc.time()
		# cat("\nLoading..")
		# Now add new dataset. 		
		# uadatasets$lst <- c(uadatasets$lst, list(read.csv(csvfilename, header=Header)))
		options(readr.show_types = FALSE) 
		#R command to open data file //, show_col_types = FALSE
		corecommand= paste('readr::read_delim(file=\'',csvfilename,'\', col_names =',Header,',delim=\'',sepCh,'\')', sep='')
		
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		#trying to open the datafile
		tryCatch({		
				withCallingHandlers({		
					eval( parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( ',corecommand,')',sep=''))) 
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
			cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}		
		
		if(success==0)
		{
		
			#03Aug2016 change "character" cols to "factor" ## This take more time to load in grid. 
			#Dataset(with many character cols loan.csv) which was taking 27sec earlier, took 1min:45sec to load when "character" changed to "factor"
			if(character.to.factor) 
			{
				eval( parse(text=paste('bskytempx <<- NULL',sep='')))## 03Aug2016 Clean x
				
				##20Jun2017 to get rid of tibble issue as.data.frame has been used below
				## Without it if you open a CSV and saveAs (other formats like .rdata, .dbf), saveAs failed. For
				## Excel it worked
				## because CSV and Excel read/write function that we are using are from Hadley. Hadley introduced 
				## tibble.			
				#eval( parse(text=paste('bskytempx <<- as.data.frame(read_csv(\'',csvfilename,'\'))',sep='')))## 03Aug2016 
				
				##following line was in use before(11Nov2021) above tryCatch
				##eval( parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( ',corecommand,')',sep=''))) #before 03Aug2016 only this line
				
				##### OLD eval( parse(text=paste(datasetname,' <<- as.data.frame( read_delim(\'',csvfilename,'\', col_names =',Header,',delim=\'',sepCh,'\'))',sep='')))

				if(logflag)
				{
					cat("\n-Before gen new col names\n")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')',sep=''))))
				}			
				
				#GenerateUniqueColName(datasetname)

				if(logflag)
				{
					cat("\n-After gen new col names\n")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')',sep=''))))
				}
				
				## 03Aug2016 We change "character" col class to "factor" ## I think Sanjay helped me to do that to Excel file. So same logic goes here
				eval(parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( lapply (.GlobalEnv$',datasetname,', function(y) {if("character" in class(y)) y = factor(y) else y} ) )',sep='' )))
				
				if(logflag)
				{
					cat("\n-After lapply new col names\n")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')',sep=''))))
				}
			}
			else
			{ 
				##20Jun2017 to get rid of tibble issue as.data.frame has been used below
				## Without it if you open a CSV and saveAs (other formats like .rdata, .dbf), saveAs failed. For 
				## Excel it worked
				## because CSV and Excel read/write function that we are using are from Hadley. Hadley introduced 
				## tibble.
				
				##following line was in use before(11Nov2021) above tryCatch
				#eval( parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame(',corecommand,')',sep=''))) #before 03Aug2016 only this line was in use. Now this is in 'else' and we added 'if'
				
				####
				######old			eval( parse(text=paste(datasetname,' <<- as.data.frame( read_delim(\'',csvfilename,'\', col_names =',Header,',delim=\'',sepCh,'\'))',sep=''))) #before 03Aug2016 only this line was in use. Now this is in 'else' and we added 'if'
				
				if(logflag)
				{
					cat("\n+Before gen new col names\n")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')',sep=''))))
				}	
				
				GenerateUniqueColName(datasetname)
				
				if(logflag)
				{
					cat("\n+After gen new col names\n")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')',sep=''))))
				}
			}		
		
			#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
			#GenerateUniqueColName(datasetname)
			# cat('\n')
			# print(eval(parse(text=paste('names(',datasetname,')[1:7]',sep=''))))
			#print(proc.time() - ptm)
			#Now replace special chars with underscore and prefix 'X' if there is digit in the first index  of colum name.
			colcount <- eval(parse(text=paste('length(names(.GlobalEnv$',datasetname,'))',sep='' ))) #colcount = length(names(datasetname))
			#cat('Count=')
			#cat(colcount)
			#ptm= proc.time()
			for( i in 1:colcount)
			{
				if(logflag)
				{
					cat('\n-----------------------------------------------------------------')
					cat("\nSpl Chr:OldName = ")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')[',i,']',sep=''))))
				}			
				eval(parse(text=paste('names(.GlobalEnv$',datasetname,')[',i,']  <- ReplaceSplChrsAndPrefixXForDigitInBegining(names(.GlobalEnv$',datasetname,')[',i,'])', sep='')))
				#names(datasetname)[i] = ReplaceSplChrsAndPrefixXForDigitInBegining(names(datasetname)[i])
				
				if(logflag)
				{
					cat("\nSpl Chr removed ColName = ")
					print(eval(parse(text=paste('names(.GlobalEnv$',datasetname,')[',i,']',sep=''))))
					cat('\n-----------------------------------------------------------------')
				}				
			}
			#print(proc.time() - ptm)
			# cat("\nLoaded")
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list
			uadatasets$name <- c(uadatasets$name, datasetname)
			#cat("\nLoaded CSV dataset :", datasetname)
			
			# if(replace == FALSE)
			# {						
				# #for old code compatibility put same list in 'name' also
				# uadatasets$name <- c(uadatasets$name, datasetname)
			# }	
						
			if(logflag)
			{
				cat("\nBefore Creating Attributes = ")
			}
			#ptm <- proc.time()
			#Creating extra attributes at column level
			UAcreateExtraAttributes(datasetname,"CSV")		
			#print(proc.time() - ptm)	
			if(logflag)
			{
				cat("\nAfter Creating Attributes = ")
			}	
		}
	}
	else
	{
		BSkyErrMsg =paste("UAreadCSV: Dataset with the same name already on the global list"," Dataset Name:", datasetname)
		warning("UAreadCSV: Dataset with the same name already on the global list ")
	}
	BSkyFunctionWrapUp()
	return(success)
}

###################################################################################################################
#Function: UAwriteCSV(csvfilename, dataSetNameOrIndex)						
#              																			
#Parameters:  
#		csvfilename				- full path filename of new CSV file to which particular dataset element 
#								  of uadatasets$lst is to be written
#		dataSetNameOrIndex     	- Dataset Name or Index (of uadatasets$lst array) which is to be 
#								  written to disk file as new CSV file.  
#
#Description:	
#		Write UA memory space dataset to a csv file format
#																						
#Example: UAwriteCSV("C:/Data/new.csv", "dietstudy")
###################################################################################################################
#' UAwriteExcel writes a in memory dataset to a disk file in Excel format
#'
#' @name UAwriteCSV
#' @param csvfilename is a full path filename of a new CSV file that is to be created.
#' @param dataSetNameOrIndex is a name of a dataset that is a reference for loaded Excel dataset.
#' @export 
UAwriteCSV <- function(csvfilename, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAwriteCSV: Error writing CSV file : ", "DataSetName :", dataSetNameOrIndex," ", "CSV filename  :", paste(csvfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAwriteCSV: Warning writing CSV file : ", "DataSetName :", dataSetNameOrIndex," ", "CSV filename  :", paste(csvfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
			##dataset saved to csv file is from uadatasets
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			
	if(!is.null(datasetname))
	{		
		#if you dont do  row.names=FALSE then default TRUE will create one extra column which make it difficult
		# to open CSV file in VirtualDyanamic 
		# eval(parse(text=paste('write.csv(',datasetname,', csvfilename,  row.names=FALSE)')))
		# above line was in use before putting tryCatch around it (below)
		
		corecommand = paste('utils::write.csv(',datasetname,', csvfilename,  row.names=FALSE)')
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
	}
	else
	{
		BSkyErrMsg =paste("UAwriteCSV:  Cannot write CSV. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		# cat("\nError: Cannot write CSV. Dataset name or index not found\n")
		warning("UAwriteCSV:  Cannot write CSV. Dataset name or index not found.")
	}			
	BSkyFunctionWrapUp()	
				
	return(success)
}

