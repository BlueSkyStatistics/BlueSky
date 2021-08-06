## Following function not at all ready
BSkyLoadSASinDataFrame <- function(SASfilename, datasetname, replace=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("BSkyLoadSASinDataFrame: Error reading SAS file : ", "DataSetName :", datasetname," ", "SAS filename  :", paste(SASfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyLoadSASinDataFrame: Warning reading SAS file : ", "DataSetName :", datasetname," ", "SAS filename :", paste(SASfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
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

		#df <- read_sas("D:/BlueSky Statistics/Misc/Data_Files/SAS/ctcodes-procedures.sas7bdat")
		eval( parse(text=paste('x <<- as.data.frame( read_sas(\'',SASfilename,'\'))', sep=''  ))) # use with lapply

		#as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )
		eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )' )))
		
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
	else
	{
		BSkyErrMsg =paste("BSkyLoadSASinDataFrame: Dataset with the same name already on the global list."," Dataset Name:", datasetname)
		warning("BSkyLoadSASinDataFrame: Dataset with the same name already on the global list ")
	}				
	BSkyFunctionWrapUp()
}
