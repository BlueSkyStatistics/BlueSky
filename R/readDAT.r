## Following will read DAT/TXT exetension (but are simple text file)
BSkyLoadDATinDataFrame <- function(DATfilename, datasetname, replace=FALSE, Header=csvHeader, sepCh='\t', deciCh='.')
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("BSkyLoadDATinDataFrame: Error reading DAT/TXT file : ", "DataSetName :", datasetname," ", "DAT/TXT filename  :", paste(DATfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyLoadDATinDataFrame: Warning reading DAT/TXT file : ", "DataSetName :", datasetname," ", "DAT/TXT filename :", paste(DATfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	##get file extension
	require(tools)
	extn = base::toupper( tools::file_ext(DATfilename))
	#cat('File Extension:')
	#print(extn)
	#cat('\n')
	
	
	##loading the dbf file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of DAT:",curidx)
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



		
		corecommand = paste('utils::read.delim(file=\'',DATfilename,'\', header =', Header,', sep = \'',sepCh,'\', dec =\'',deciCh,'\' )',sep='' )
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		#trying to open the datafile
		tryCatch({
		
				withCallingHandlers({					
					eval( parse(text=paste('x <<-  utils::read.delim(file=\'',DATfilename,'\', header =', Header,', sep = \'',sepCh,'\', dec =\'',deciCh,'\' )',sep='' ))) # use with lapply

					#as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )
					eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )' )))
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

		
		if(success==0)
		{
			#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
			GenerateUniqueColName(datasetname)
			
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list
			# names(uadatasets$lst)[length(uadatasets$lst)]<-datasetname
			uadatasets$name <- c(uadatasets$name, datasetname)
			#cat("\nLoaded DAT dataset :", datasetname)					
			
			# if(replace == FALSE)
			# {						
				# #for old code compatibility put same list in 'name' also
				# uadatasets$name <- c(uadatasets$name, datasetname)
			# }	
						
			# DataSetIndex <- UAgetIndexOfDataSet(datasetname)


			#Creating extra attributes at column level
			if(extn=='DAT')
				UAcreateExtraAttributes(datasetname, "DAT")					
			else if (extn=='TXT')
				UAcreateExtraAttributes(datasetname, "TXT")	
			else if (extn=='CSV')
				UAcreateExtraAttributes(datasetname, "CSV")		#if Mkt Bas data file with CSV extension.	
		}			
	}
	else
	{
		BSkyErrMsg =paste("BSkyLoadDATinDataFrame: Dataset with the same name already on the global list."," Dataset Name:", datasetname)
		warning("BSkyLoadDATinDataFrame: Dataset with the same name already on the global list ")
	}				
	BSkyFunctionWrapUp()
}
