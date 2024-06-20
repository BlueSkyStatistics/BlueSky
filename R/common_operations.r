######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAgetIndexOfDataSet
# UAgetIndexOfColInDataSet
# UAgetIndexsOfColsInDataSet
# UAgetColData
# UAgetRowsOfColByIndex
# UAgetRowsByIndex
# BSkyDatasetExists
# BSkyColumnExists
# BSkyGetColNumericFactors
# BSkyScaleToNominalOrOrdinal
# BSkyNominalOrOrdinalToScale
# BSkyOrdinalToScale
# BSkyAttributesBackup
# BSkyAttributesRestore
# BSkyGetFactorMap
# BSkyGetLevelIndex
# BSkyFunctionExists
# BSkyGetFilenameOrPath
# BSkyFilename2Datasetname
# BSkyValidateDataset
# BSkyValidateDatasetFromSlice
# BSkyValidateColumn
# ExtractDatasetNameFromGlobal
#################################################################################################


# UAgetIndexOfDataSet UAgetColNames UAsetColDesc UAgetColAlign UAsetColAlign UAgetColLevelSplit UAsetColLevelSplit UAgetColData UAgetRowsOfColByIndex
# UAgetRowsByIndex  BSkyDatasetExists  BSkyColumnExists  BSkyGetColNumericFactors  BSkyScaleToNominalOrOrdinal  BSkyNominalOrOrdinalToScale
# BSkyOrdinalToScale BSkyGetFactorMap BSkyGetLevelIndex  BSkyFormat  BSkyFunctionExists BSkyGetFilenameOrPath BSkyFilename2Datasetname

# Global env vars
#cat("\nInitializing the global variable to hold the list of open datasets\n")
#uaOpenDataSetPackageEnv <-NULL
#uaOpenDataSetPackageEnv <- new.env(parent=globalenv())
#uaOpenDataSetPackageEnv$uaDataSetList<-list()
#uadatasets$lst

#############################################################################################################
#Function: UAgetIndexOfDataSet(datasetname)						
#              																							
#Parameters: 
#		datasetname			- Dataset which is to be searched in global environment.
#																											
#Description:	
#		Returns the index of dataset if exists, else returns -1 
#																											
#Example: UAgetIndexOfDataSet("mydataset")																								
#																											
#############################################################################################################

UAgetIndexOfDataSet <- function(datasetname)
{
 #BSkyFunctionInit()
 #BSkySetCurrentDatasetName(datasetname)
	
 #BSkyErrMsg = paste("UAgetIndexOfDataSet: Error getting index of dataset : ", "DataSetName :", datasetname,sep="")
 #BSkyWarnMsg = paste("UAgetIndexOfDataSet: Warning getting index of dataset : ", "DataSetName :", datasetname,sep="")
 #BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

 ###19Oct2016 Adding a logic below. If .GlobalEnv$Dataset1 is passed it will be trimmed to Dataset1
 		#if datasetname is of the format .GlobalEnv$datasetname then strip out datasetname first
		#####ptm= proc.time()
		if(is.character(datasetname))
		{
			charcount <- nchar(datasetname)
			if(charcount > 11) # .GlobalEnv$
			{
				if(substr(datasetname, 1,11) == ".GlobalEnv$")
				{
					datasetname = substr(datasetname, 12,charcount)
				}
			}
		}
 
 
	indexofdataset <- -1
	if(length(uadatasets$name) > 0)
	{
		idx <- which(datasetname==uadatasets$name)
		
		if(length(idx)>0)
		{
			#data set found matching the name <datasetname>
			#cat("\nDataSet Found ! ")
			indexofdataset = idx #-return(i)
		}
		else
		{
			#Error: dataset does not exist
			#cat("\nDataSet Not Found ! ")
			BSkyErrMsg =paste("UAgetIndexOfDataSet: Dataset not already loaded"," Dataset Name:", datasetname)
			#warning("UAgetIndexOfDataSet: Dataset not already loaded")
			#-return(-1)
		}
	}
 #BSkyFunctionWrapUp()
	#print(BSkyReturnStructure())
	#cat("Returning return structure from this top level sort function\n")
	return(invisible(indexofdataset)) #	
}

#### Finding col index in a dataSet("mydata"). Col can be say "age". and index is 3
# 
#############################################################################################################
#Function: UAgetIndexOfColInDataSet(datasetname, colName)						
#              																							
#Parameters: 
#		datasetname	- Index of dataset
#		colName				- Column name whose index is to be found.
#																											
#Description:	
#		Returns the index of column, if exists, else return -1
#																											
#Example: UAgetIndexOfColInDataSet("mydataset","gender")																					
#																											
#############################################################################################################
UAgetIndexOfColInDataSet<-function(datasetname,colName)#DONE
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	# BSkyErrMsg = paste("UAgetIndexOfColInDataSet: Error getting column index : ", "DataSetName :", datasetname," ", "Variable  :", paste(colName, collapse = ","),sep="")
	# BSkyWarnMsg = paste("UAgetIndexOfColInDataSet: Warning getting column index : ", "DataSetName :", datasetname," ", "Variable :", paste(colName, collapse = ","),sep="")
	# BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
#cat('Col Name:')
#cat(colName)
	indexofcol = -1
	##30jul2013 No need of following 'if' this function is a sub function and top function can varify for valid dataset.
	# if(UAgetIndexOfDataSet(datasetname) > 0 ) ##Verifying if dataset is valid
	# {
		#find index of col
		idx <- eval(parse(text=paste('which(colName == colnames(',datasetname,') )',sep='')))
		#idx <- eval(parse(text=paste('which(colName == colnames(.GlobalEnv$',datasetname,') )',sep='')))
#cat("\n Col index after searching..\n")
#print(idx)
		if(length(idx) > 0) ### if index found
		{
			indexofcol <- idx
			#cat("\nIndex of ",colName,"-col:",indexofcol,"\n")
		}
		else
		{
			#cat("\nUAgetIndexOfColInDataSet:Col not Found: '", colName,"' Index=", idx,"\n")
			#Error: index of col does not exist
			BSkyErrMsg =paste("UAgetIndexOfColInDataSet: Index not found."," Col Name:", colName)
			warning("UAgetIndexOfColInDataSet: Column not found.")
			BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
			#-return(-1)
		}					
	# }
	# else
	# {
		# cat("\nUAgetIndexOfColInDataSet:Dataset not found\n")
		# #Error:  Global variable seems to have been removed by accident
		# BSkyErrMsg =paste("UAgetIndexOfColInDataSet: Dataset not found."," Dataset Name:", datasetname)
		# warning("UAgetIndexOfColInDataSet: Dataset not found."," Dataset Name:", datasetname)
		# BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
		# #-return(-1)
	# }
	BSkyFunctionWrapUp()				
	return(invisible(indexofcol))
}



#############################################################################################################
#Function: UAgetIndexsOfColsInDataSet(indexOfDataSetName, colName)						
#              																							
#Parameters: 
#		indexOfDataSetName		- Index or name of dataset
#		colNames				- Column names whose indexes are to be found.
#																											
#Description:	
#		Returns the index of column which is in the dataset whose name is specified.
#																											
#Example: UAgetIndexsOfColsInDataSet("mydataset",colnames)																					
#																											
#############################################################################################################
UAgetIndexsOfColsInDataSet<-function(dataSetNameOrIndex,colNames) #DONE
{
	BSkyFunctionInit()
	#cat("\nEntring UAgetIndexsOfColsInDataSet \n")
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	#cat('\nBefore:DS name in UAgetIndexsOfColsInDataSet ')
	#print(dataSetNameOrIndex)
	#print(colNames)
	# BSkyErrMsg = paste("UAgetIndexsOfColsInDataSet: Error getting col indexes : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNames, collapse = ","),sep="")
	# BSkyWarnMsg = paste("UAgetIndexsOfColsInDataSet: Warning getting col indexes  : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNames, collapse = ","),sep="")
	# BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
				indexes <- c()
				#uaPackageEnv<- parent.env(environment())
				
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	#cat('\nAfter:DS name in UAgetIndexsOfColsInDataSet ')
	#print(datasetname)
	
			if(!is.null(datasetname))
			{		
				#Error: if DataSetIndex is greater than the total dataset loaded currently. Out of Bound	
					# if dataset is not empty
					noOfColInDataSet= eval(parse(text=paste('ncol(',datasetname,')',sep='')))
					if(noOfColInDataSet > 0)
					{

						# cat("\nNo. Of Cols :>", noOfColInDataSet, "\n")
						# loop till, the cols are not found in list

						for(j in 1:length(colNames))
						{
							 #cat("\nin Loop Col name:",colNames[j])
							 idx <- UAgetIndexOfColInDataSet(datasetname,colNames[j])
							 #cat("\n Index:",idx,"\n")
							 if(idx > 0)
							 {
								indexes <- c(indexes, idx)
							 }
							 else
							 {
								BSkyErrMsg =paste("UAgetIndexsOfColsInDataSet:  Col does not exists."," Col Name:'", colNames[j],"'")
								#warning("UAgetIndexsOfColsInDataSet:  Col does not exists.")
								BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
							 }
							
							# i <- 1
							# while(i <= noOfColInDataSet ){
								# if(names(datasetname[i])== colNames[j])
								# {
									# # cat("\nIndx:",i)
									# #cat("\t Name:")
									# #print(uaPackageEnv$lst[[DataSetIndex]][i])
									# if(length(indexes) == 1 && indexes == -1)
										# indexes <- c(i)
									# else
										# indexes <- c( c(indexes), i)
								# }
								# i<- i+1;
							# }
							# cat("\nIndexes:")
							# print(indexes)
						}
						# cat("\nIndexs found:")
						# print(length(indexes))
						##cat("\nCol index =  ", i, " - ", colName, "\n")
						# if found i should be less or equal to list size.
						if(length(indexes)>0){
							#data set found matching the name <DataSetIndex>
							#-return(indexes)
						}
						else{
							#BSkyErrMsg = Error: dataset does not exist
							BSkyErrMsg =paste("UAgetIndexsOfColsInDataSet:  Columns does not exist.")
							warning("UAgetIndexsOfColsInDataSet:  Columns does not exist.")
							BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
							#-return(-1)
						}
					}
					else{
						#Error: datasetList is empty
						BSkyErrMsg =paste("UAgetIndexsOfColsInDataSet:  Dataset with no columns."," Dataset Name:", datasetname)
						warning("UAgetIndexsOfColsInDataSet:  Dataset with no columns.")
						BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
						#-return(-1)
					}
			}
			else
			{
				# cat("\nError: Cannot get index of multiple cols. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetIndexsOfColsInDataSet: Cannot get index of multiple cols. Dataset name not found"," Dataset Name:", datasetname)
				warning("UAgetIndexsOfColsInDataSet: Cannot get index of multiple cols. Dataset name not found")
				BSkyStoreApplicationWarnErrMsg(BSkyErrMsg, BSkyErrMsg)
			}				
		BSkyFunctionWrapUp()
		#cat('\nIndex :')
		#print(indexes)
		#cat("\nLeaving UAgetIndexsOfColsInDataSet \n")
	return(invisible(indexes))
}


#############################################################################################################
#Function: UAgetColData(colNameOrIndex, dataSetNameOrIndex)											
#              																			
#Parameters:     
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose col data is needed         																
#																						
#Description:
#		Returns the complete data that belongs to a specific column																			
#																						
#Example: UAgetColData("gender", "mydataset")																				
#																						
#############################################################################################################
UAgetColData <- function(colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetColData: Error getting col data : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetColData: Warning getting col data  : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	col.int.vector <- ""
	tryCatch(
		{

		withCallingHandlers(
		{
			##cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				##cat("\nFound dataset index:",DataSetIndex,"\n")
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)					
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				##cat(colIndex,"\n:-) ")
				if(colIndex > 0){
					col.int.vector <- eval(parse(text=paste('unclass(',datasetname,'[[',colIndex,']])',sep='')))
					#-return(col.int.vector)
				}		
				else
				{
					# cat("\nError: Cannot get col data. Col not found\n")
					BSkyErrMsg =paste("UAgetColData: Cannot get col data. Col not found"," Col Name:", colNameOrIndex)
					warning("UAgetColData: Cannot get col data. Col not found")
				}				

			}
			else
			{
				# cat("\nError: Cannot get col data. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColData: Cannot get col data. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAgetColData: Cannot get col data. Dataset name or index not found")
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in UAgetColData \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetColData \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(col.int.vector)))))
}


#############################################################################################################
#Function: UAgetRowsOfColByIndex(colNameOrIndex, startRow, numOfRows, dataSetNameOrIndex)  								
#              																								
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose rows(records) are needed   
#		startRow					- Row index, which will be considered as starting point for fetching records.
#		numOfRows					- Number of rows to fetch starting from index specified by 'startRow' parameter.
#																											
#Description:
#		Returns specified number of records from specified location, of a specified column from specified 
#		dataset																								
#																											
#Example: UAgetRowsOfColByIndex("gender", 15, 5, "mydataset")																								
#																											
#############################################################################################################   
UAgetRowsOfColByIndex <- function(colNameOrIndex, startRow, numOfRows, dataSetNameOrIndex) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetRowsOfColByIndex: Error getting selected range values from a col : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetRowsOfColByIndex: Warning getting selected range values from a col : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	col.int.vector <- ""
	tryCatch(
		{

			withCallingHandlers(
			{	
				
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
				
				if(!is.null(datasetname))
				{		
					#Error: dataSetName not found
					uaPackageEnv<-uadatasets
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
					# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
					if(colIndex > 0){
						totalRows <- eval(parse(text=paste('length(',datasetname,'[[1]])',sep='')))
						#cat("Total Rows:", totalRows,"\n")	
						#Warning: if colIndex is greater than the total number of records then display a warning
						endRow <- startRow + numOfRows  - 1
						if(startRow < 1 || startRow > totalRows){
						  #Error: throw error message and skip execution of this function
						}
						#if endRow is greater than number of total records then endrow = total_recs
						if( endRow > totalRows){
							endRow <- totalRows
						}
						col.int.vector <- eval(parse(text=paste('unclass(',datasetname,'[[',colIndex,']][c(',startRow,':',endRow,')])')))
					}
					else
					{
						# cat("\nError: Cannot get rows of a col by index. Col not found\n")
						BSkyErrMsg =paste("UAgetRowsOfColByIndex: Cannot get rows of a col by index. Col not found"," Column Name:", colNameOrIndex)
						warning("UAgetRowsOfColByIndex: Cannot get rows of a col by index. Col not found")
					}

				}
				else
				{
					# cat("\nError: Cannot get rows of a col by index. Dataset name or index not found\n")
					BSkyErrMsg =paste("UAgetRowsOfColByIndex: Cannot get rows of a col by index. Dataset name or index not found"," Dataset Name:", datasetname)
					warning("UAgetRowsOfColByIndex: Cannot get rows of a col by index. Dataset name or index not found")
				}			
			},
				
			warning = UAwarnHandlerFn

			) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)	
	#-return(col.int.vector)
		if(BSkyLocalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in UAgetRowsOfColByIndex \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetRowsOfColByIndex \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(col.int.vector)))))
}


#############################################################################################################
#Function: UAgetRowsByIndex(startRow, numOfRows, dataSetNameOrIndex)												
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		startRow					- Row index, which will be considered as starting point for fetching records.
#		numOfRows					- Number of rows to fetch starting from index specified by 'startRow' parameter.            																					
#																											
#Description:
#		Returns specified number of records from specified begin location; from all columns of specified dataset.
#																											
#Example: UAgetRowsByIndex(5 , 3, "mydataset")																								
#																											
#############################################################################################################
UAgetRowsByIndex <- function(startRow, numOfRows, dataSetNameOrIndex) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetRowsByIndex: Error getting number of records : ", "DataSetName :", dataSetNameOrIndex," ", "Start row  :", paste(startRow, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetRowsByIndex: Warning getting number of records : ", "DataSetName :", dataSetNameOrIndex," ", "Start row :", paste(startRow, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	selRows <- ""
	tryCatch(
		{
		
			withCallingHandlers(
			{					
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
				
				if(!is.null(datasetname))
				{		
					#Error: invalid dataSetName (not found)

					totalRows <- eval(parse(text=paste('nrow(',datasetname,')',sep='')))
					#cat("Total Rows:", totalRows,"\n")
					noOfCols <- eval(parse(text=paste('ncol(',datasetname,')',sep='')))
					endRow <- startRow + numOfRows - 1
					if(startRow < 1 || startRow > totalRows){
					  #Error: throw error message and skip execution of this function
					}
					#if endRow is greater than number of total records then endrow = total_recs
					if( endRow > totalRows){
						endRow <- totalRows
					}
					i=1
					#Creating a matrix to store the specified number of records
					selRows<-matrix(,nrow=numOfRows,ncol=noOfCols)
					#putting the data col by col in selRows
					while(i <= noOfCols){
						#putting specified number of records of a particular col (say gender; coded as c(i)) in one col of selRows
						selRows[,i] <- eval(parse(text=paste(datasetname,'[[c(',i,')]][c(',startRow,':',endRow,')]',sep='')))
						i <- i + 1
					}
				}
				else
				{
					# cat("\nError: Cannot get rows(all cols) by index. Dataset name or index not found\n")
					BSkyErrMsg =paste("UAgetRowsByIndex: Cannot get rows(all cols) by index. Dataset name or index not found"," Dataset Name:", datasetname)
					warning("UAgetRowsByIndex: Cannot get rows(all cols) by index. Dataset name or index not found")
				}			

			},
				
			warning = UAwarnHandlerFn

			) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
#-return(selRows)
	if(BSkyLocalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in UAgetRowsByIndex \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetRowsByIndex \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(selRows))))) 
}

## See if datasets exists if its index is provided ##
BSkyDatasetExists<-function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyDatasetExists: Error finding dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyWarnMsg = paste("BSkyDatasetExists: Warning finding dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	DatasetExists <- FALSE

datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!is.null(datasetname))
	{
		DatasetExists <- TRUE #-return(TRUE)
	}
	else
	{
		DatasetExists <- FALSE #-return(FALSE)
	}



		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(DatasetExists))# return(BSkyReturnStructure(DatasetExists))
}

## See if column exists in datasets ##
BSkyColumnExists <- function(dataSetNameOrIndex, ColNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyColumnExists: Error finding col : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(ColNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyColumnExists: Warning finding col : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(ColNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	colExists <- FALSE

datasetname <- BSkyValidateDataset(dataSetNameOrIndex)	
		if(!is.null(datasetname))
		{
			#Find is colname exists
colIndex <- BSkyValidateColumn(dataSetNameOrIndex, ColNameOrIndex)	
			noOfCols <- eval(parse(text=paste('ncol(',datasetname,')',sep='')))
			if(colIndex <= noOfCols && colIndex > 0)#Col exisits
			{
				#- return(TRUE)
				colExists <- TRUE
			}
			else
			{
				#- return(FALSE)
				colExists <- FALSE
			}
		}
		else
		{
			#- return(FALSE)
			colExists <- FALSE
		}

		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(colExists)) #return(BSkyReturnStructure(colExists))
}


BSkyGetColNumericFactors <- function(ColNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyGetColNumericFactors: Error getting col numeric factors : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(ColNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyGetColNumericFactors: Warning getting col numeric factors : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(ColNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			numFactors <- 0
			tryCatch(
		{
	
		withCallingHandlers(
		{


datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		if(!is.null(datasetname))
		{
			#Find is colname exists
colIndex <- BSkyValidateColumn(datasetname, ColNameOrIndex)	
			if(colIndex > 0)#Col exisits
			{
				# numFactors <- 0
				faclen <- eval(parse(text=paste('length(levels(factor(',datasetname,'[[',colIndex,']])))',sep='')))
				if(faclen > 0)
					numFactors <- eval(parse(text=paste('levels(factor(',datasetname,'[[',colIndex,']]))',sep='')))
				#-return(numFactors)
			}
			else
			{
				BSkyErrMsg =paste("BSkyGetColNumericFactors: Col not found"," Col Name:", ColNameOrIndex)
				warning("BSkyGetColNumericFactors: Col not found")
				#-return(0)
			}
		}
		else
		{
			BSkyErrMsg =paste("BSkyGetColNumericFactors: Dataset name not found"," Dataset Name:", datasetname)
			warning("BSkyGetColNumericFactors: Dataset name or index not found")
			#-return(0)
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyGetColNumericFactors \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyGetColNumericFactors \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(numFactors)))))
}

# numericValues - c( 1,2 ,4,7)  ### Dataset col Values matching to these will be replaced by factor name and non-matching should change to NA
# levelNames  -> c("Best", "Better", "Good", "Bad")
# changeto -> "Nominal" or "Ordinal"
BSkyScaleToNominalOrOrdinal <- function(colNameOrIndex, numericValues, levelNames, changeto, dataSetNameOrIndex) ## working OK
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyScaleToNominalOrOrdinal: Error converting scale to nominal or ordinal : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyScaleToNominalOrOrdinal: Warning converting scale to nominal or ordinal : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
		tryCatch(
		{
	
		withCallingHandlers(
		{


	#1 check if col is factor. It should be then only we can add levels to it
	#2 add new levels. levels(uadatasets$lst[[1]]$colname)<- c("l1","l2","l3")
	##cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
			#cat("\nAttr Backed up \n")
			bskyattrs <- BSkyAttributesBackup(colIndex, datasetname)##backup all attributes
			# print(bskyattrs)
			
			if(changeto == "Nominal") ## Scale to Nominal
			{
				#### Following comments; progressing line by line  ####
				# paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'], levels=',numericValues,', labels=\'',levelNames,'\')',sep='')
				# paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'], levels=',paste(numericValues,collapse=','),', labels=',paste(levelNames,collapse=','),')',sep='', collapse=NULL)
				# paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'], levels=',paste(numericValues,collapse=','),', labels=c(',paste("'",levelNames,"'",sep='',collapse=','),'))',sep='', collapse=NULL)
eval(parse(text=paste(datasetname,'[,',colIndex,'] <- factor(',datasetname,'[,',colIndex,'], levels=c(',paste(numericValues, sep='',collapse=','),'), labels=c(',paste("'",levelNames,"'",sep='',collapse=','),'))',sep='', collapse=NULL)))#. <<- to <-

			}
			else if(changeto == "Ordinal") ## Scale to Ordinal
			{
				# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- ordered(',datasetname,'[,',colIndex,'], levels=',numericValues,', labels=\'',levelNames,'\')',sep='')))
eval(parse(text=paste(datasetname,'[,',colIndex,'] <- ordered(',datasetname,'[,',colIndex,'], levels=c(',paste(numericValues, sep='',collapse=','),'), labels=c(',paste("'",levelNames,"'",sep='',collapse=','),'))',sep='', collapse=NULL)))#. <<- to <-	
			}
			else if(changeto == "Scale") ## Nominal, Ordinal to Scale
			{
				##after successfully testing BSkyNominalOROrdinalTOScale(), copy that here and delete that function
			}	
			BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes

			## store factor mapping for restore in future ##
			eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$numVals <- ',numericValues,sep='')))#. <<- to <-
			eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$levNames <- \'',levelNames,'\'',sep='')))#. <<- to <-		
		}
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyScaleToNominalOrOrdinal \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyScaleToNominalOrOrdinal \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure()))
}

BSkyNominalOrOrdinalToScale <- function(colNameOrIndex, numericValues, levelNames, changeto, dataSetNameOrIndex) ## Last value is duplicated, else working OK
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyNominalOrOrdinalToScale: Error  converting nominal ordinal to scale : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyNominalOrOrdinalToScale: Warning  converting nominal ordinal to scale : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
		tryCatch(
		{
	
		withCallingHandlers(
		{


datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
		#Error: dataSetName and colname not found
		#cat("Start Conv Nom 2 Scl")
		if(colIndex > 0)
		{
			## Nominal, Ordinal to Scale
			if(changeto == "Scale") 
			{
				##backup all attributes
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname)
				#cat("Confirmed..Scale")
#eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor("")',sep=''))) #for new dataset it may be good but for existing one it may be harmful
				##convert Nominal Or Ordinal to Scale
				#eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- as.numeric(',datasetname,'[,',colIndex,'])',sep='')))
				eval(parse(text=paste(datasetname,'[,',colIndex,'] <- as.character(',datasetname,'[,',colIndex,'])',sep='')))#. <<- to <-
				##get current numric values of col which is now scale. Values will be 1,2,3,...
				currentnumericval <- eval(parse(text=paste('levels(factor(',datasetname,'[,',colIndex,']))',sep='')))
				#print(currentnumericval)
				#cat("\n")
				#print(numericValues)
				#levcount = length(currentnumericval)
				#eval(parse(text=paste('class(',datasetname,'[,',colIndex,']) <<- "integer"',sep=''))) #fix for datagrid to hv numeric back in cells while doing S2N > N2S process
				## Replace corressponding current values (1,2,3,...) with the one those are sent from UI
				## lapply / sapply or /replicate
				## replicate(a, numericVal)
				i=1
				##for(i in 1:levcount)#### fix small bug #### accid > Nominal > Scale then there is duplicate value.
				{
				#eval(parse(text=paste(datasetname,'[,',colIndex,'][ ',datasetname,'[,',colIndex,'] == ',currentnumericval,' ] <<- ',numericValues,sep='')))
				#eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- ',datasetname,'[,',colIndex,'][ ',datasetname,'[,',colIndex,'] == "',levelNames,'" ] <<- ',numericValues,sep='')))				
				eval(parse(text=paste(datasetname,'[,',colIndex,'][ ',datasetname,'[,',colIndex,'] == "',levelNames,'" ] <- ',numericValues,sep='')))#. <<- to <-				
				#print(paste(datasetname,'[,',colIndex,'] <<- ',datasetname,'[,',colIndex,'][ ',datasetname,'[,',colIndex,'] == "',levelNames,'" ] <<- ',numericValues,sep=''))
				}
				## restore all attributes
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)
				
				## stored new copy of factor mapping ##
				eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$numVals <- ',numericValues,sep='')))#. <<- to <-
				eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$levNames	<- \'',levelNames,'\''	,sep='')))#. <<- to <-
								
				#cat("\nDone N O to S")
			}
		}
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyNominalOrOrdinalToScale \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyNominalOrOrdinalToScale \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure()))
}

BSkyOrdinalToScale <- function(colNameOrIndex, factorNames, dataSetNameOrIndex)
{
	tryCatch(
		{
	
		withCallingHandlers(
		{

#############################

		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution	
	
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
}

BSkyAttributesBackup <- function(colIndex, datasetname)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("BSkyAttributesBackup: Error attributes backup : ", "DataSetIndex :", datasetname," ", "Variable  :", paste(colIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyAttributesBackup: Warning attributes backup : ", "DataSetIndex :", datasetname," ", "Variable :", paste(colIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	#### colNamesOrIndex, dataSetNameOrIndex are already valid. Calling function will do that varificaiton. ####	
	dsattrs = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])',sep='')))
	# coldesc = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$coldesc',sep='')))
	# usermissing = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$usermissing',sep='')))
	# split = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$split',sep='')))
	# levelLabels = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$levelLabels',sep='')))
	# width = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$width',sep='')))
	# columns = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$columns',sep='')))
	# align = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$align',sep='')))
	# measure = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$measure',sep='')))
	# role = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$role',sep='')))
	# label = eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$label',sep='')))
	colname = eval(parse(text=paste('colnames(',datasetname,')[',colIndex,']',sep=''))) ## this can be used while restoring attr
	coldesc = dsattrs$coldesc
	usermissing = dsattrs$usermissing
	split = dsattrs$split
	levelLabels = dsattrs$levelLabels
	width = dsattrs$width
	columns = dsattrs$columns
	align = dsattrs$align
	measure = dsattrs$measure
	role = dsattrs$role
	label = dsattrs$label
	if(is.null(label)){
		label=""
	}
	

	bskyattlist <- list(at1 = coldesc,
		at2 = usermissing,
		at3 = split,
		at4 = levelLabels,
		at5 = width,
		at6 = columns,
		at7 = align,
		at8 = measure,
		at9 = role,
		at10 = label,
		at11 = colname)
		
	BSkyFunctionWrapUp()
	return(invisible(bskyattlist))

}

BSkyAttributesRestore <- function(colIndex, bskyattlist, datasetname) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	# cat("\nRestoring Attr..")
	BSkyErrMsg = paste("BSkyAttributesRestore: Error restoring attributes: ", "DataSetIndex :", datasetname," ", "Variable  :", paste(colIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyAttributesRestore: Warning restoring attributes: ", "DataSetIndex :", datasetname," ", "Variable :", paste(colIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	#cat("\nRestoring Attr..now")
									#### using setattr()  ####
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$coldesc <- bskyattlist$at1',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$usermissing <- bskyattlist$at2',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$split <- bskyattlist$at3',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$levelLabels <- bskyattlist$at4',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$width <- bskyattlist$at5',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$columns <- bskyattlist$at6',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$align <- bskyattlist$at7',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$measure <- bskyattlist$at8',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$role	 <- bskyattlist$at9',sep='')))# <<- to <- coz .GlobalEnv
	eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$label <- bskyattlist$at10',sep='')))# <<- to <- coz .GlobalEnv

									#### using setattr()  ####
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"coldesc"' , ', "', bskyattlist$at1,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"usermissing"' , ', "', bskyattlist$at2,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"split"' , ', "', bskyattlist$at3,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"levelLabels"' , ', "', bskyattlist$at4,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"columns"' , ', "', bskyattlist$at5,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"columns"' , ', "', bskyattlist$at6,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"align"' , ', "', bskyattlist$at7,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"measure"' , ', "', bskyattlist$at8,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"role"' , ', "', bskyattlist$at9,'")',sep='')))
	# eval(parse(text=paste('setattr(',datasetname,'[[',colIndex,']],','"label"' , ', "', bskyattlist$at10,'")',sep='')))
	#cat("\nRestored Attr..")
	BSkyFunctionWrapUp()
	return(invisible())
}

##TOP SUB##
BSkyGetFactorMap <- function(colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	BSkyErrMsg = paste("BSkyGetFactorMap: Error in getting factor map : ", "DataSetName :", dataSetNameOrIndex," ", "Col name or index :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyGetFactorMap: Warning in getting factor map : ", "DataSetName :", dataSetNameOrIndex," ", "Col name or index :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	factormap <- list()
	tryCatch(
		{
	
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
			##check if attributes exists else return current string names with integers 1,2,3... as factor map
			levelnames <- eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$levNames',sep='')))
			numericvalues <- eval(parse(text=paste('attributes(',datasetname,'[[',colIndex,']])$numVals',sep='')))
			if( length(levelnames) < 1)
			{
				## for scale type: levels(factor(uadatasets$lst[[DataSetIndex]][[colIndex]]))
				levelnames <- eval(parse(text=paste('levels(',datasetname,'[[',colIndex,']])',sep='')))
				nooflevels <- length(levelnames)
				numericvalues <- c(1:nooflevels)
			}

			factormap <- list(lvlnames=c(levelnames), numvalues=c(numericvalues))
			#factormap <- data.frame(levelnames, numericvalues)
			#-return(factormap)
			
		}
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyGetFactorMap \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyGetFactorMap \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(factormap)))))
}

##TOP SUB##
#BSkyGetLevelIndex( 2, "Male", "acc")
#levelname is string name for level. This function will return numeric value for string levelname
BSkyGetLevelIndex <- function(colIndex, levelname, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyGetLevelIndex: Error getting level index for level name : ", "DataSetName :", dataSetNameOrIndex," ", "Colindex & Levelname:", paste(colIndex,levelname, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyGetLevelIndex: Warning getting level index for level name : ", "DataSetName :", dataSetNameOrIndex," ", "Colindex & Levelname :", paste(colIndex,levelname, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			levelindex =0
		tryCatch(
		{
	
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		
colIndex <- BSkyValidateColumn(datasetname, colIndex)	
	# allLevels = levels(datasetname[[colIndex]])
	# noOfLevels = length(allLevels) #levels(uadatasets$lst[[dataSetIndex]][[colIndex]])

	idx <- eval(parse(text=paste('which(',levelname,' == levels(',datasetname,'[[',colIndex,']]))',sep='')))
	if(length(idx) > 0)
	{
		levelindex = idx
	}
	#-return(0)
	}##if
			},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution	
	
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
		if(BSkyLocalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyGetLevelIndex \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get level index function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyGetLevelIndex \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get level index function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(levelindex)))))
}



BSkyFunctionExists<-function(funcname)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyFunctionExists: Error: Function Does not exists : ", "Function Name :", funcname,sep="")
	BSkyWarnMsg = paste("BSkyFunctionExists: Warning : Function Does not exists : ", "Function Name :", funcname,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	funcExists <- FALSE
	#ualist <- ls("package:ua")
	uadatapacklist <- ls("package:uadatapackage")
	result = grep(funcname, ualist)
	# if(length(result)==0)
	# {
		result = grep(funcname, uadatapacklist)
		if(length(result)>=0)
		{
		     funcExists <- TRUE #- return(TRUE)
		}
		else
		{
			BSkyWarnMsg =paste("BSkyFunctionExists: Function does not exists.")
			warning("BSkyFunctionExists: Function does not exists.")
		}
		
	# }
	BSkyFunctionWrapUp()
	return(invisible(funcExists))
}

## This functions takes a fullpath filename and returns the file name or file path based on the pathflag. path=TRUE returns path elseit returns filename
BSkyGetFilenameOrPath<-function(fullpathfilename, path=FALSE)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyGetFilenameOrPath: Error in getting filename or path : ", "Fullpath filename :", fullpathfilename," ", "Getting Path :", paste(path, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyGetFilenameOrPath: Warning in getting filename or path : ", "Fullpath filename :", fullpathfilename," ", "Getting Path :", paste(path, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	nameobj <- ""
			# cat("\nFullpath Filename :", fullpathfilename)
			if(path)
			{
				nameobj<-dirname(fullpathfilename) ## its a stripped off path from fullpathfilename
			}
			else
			{
				nameobj<-basename(fullpathfilename)## its a stripped off filename from fullpathfilename
			}
			# cat("\nFile or Path :", nameobj)
	BSkyFunctionWrapUp()
			return(invisible(nameobj))
			
}

##Pass a filename and get dataset name
BSkyFilename2Datasetname<-function(nopathfilename)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyFilename2Datasetname: Error in converting Filename to Datasetname : ", "Filename :", nopathfilename,sep="")
	BSkyWarnMsg = paste("BSkyFilename2Datasetname: Warning in converting Filename to Datasetname : ", "Filename :", nopathfilename,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
			#cat("\nNopath File Name:",nopathfilename)
			### empty spaces within filename will be removed. Later depending upon requirement remove special chars too.
			datasetnamewithextension <- gsub(" ", "", nopathfilename )
			#datasetname <- gsub(".sav","",gsub(" ", "", nopathfilename ))
			datasetnamewithoutextension <- sub("^([^.]*).*", "\\1", datasetnamewithextension) 
			#cat("\nReturning Dataset Name without extension:",datasetnamewithoutextension)
		BSkyFunctionWrapUp()			
			return(invisible(datasetnamewithoutextension))
}

## Pass Dataset name or its index ( in 'name' )
## Returns Dataset name if its valid/exists else returns null. Datasetname is needed for using it in EVAL(PARSE(PASTE()))
BSkyValidateDataset <- function(dataSetNameOrIndex)
{
		#cat(' 1.BSkyValidateDataset : ')
		#cat(dataSetNameOrIndex)
		#if dataSetNameOrIndex is of the format .GlobalEnv$datasetname then strip out datasetname first
		#####ptm= proc.time()
		if(is.character(dataSetNameOrIndex))
		{
			charcount <- nchar(dataSetNameOrIndex)
			if(charcount > 11) # .GlobalEnv$
			{
				if(substr(dataSetNameOrIndex, 1,11) == ".GlobalEnv$")
				{
					dataSetNameOrIndex = substr(dataSetNameOrIndex, 12,charcount)
				}
			}
		}
		#####print(proc.time() - ptm)
		#cat(' 2.BSkyValidateDataset : ')
		#cat(dataSetNameOrIndex)
		
			datasetname <- NULL
			if(is.numeric(dataSetNameOrIndex)) ## if numeric
			{
				#####ptm= proc.time()
				datasetname <- BSkyValidateDatasetFromSlice(dataSetNameOrIndex) ## first find datasetname for slice index
				
				if(is.null(datasetname))  ##if not found then look in dataset loaded outside
				{
					idx <- dataSetNameOrIndex
					totalds = length(uadatasets$name) 
					if(idx <= totalds) 			
						datasetname = uadatasets$name[idx]
				}
				#cat('\nDataset name in BSkyValidateDataset:')
				#print(datasetname)
				#####print(proc.time() - ptm)
			}
			else ## if char dataset name is given
			{

				DatasetIndex <- UAgetIndexOfDataSet(dataSetNameOrIndex)
				if(DatasetIndex > 0)
					datasetname <- dataSetNameOrIndex

			}
			#cat("\nDataset Name Validation result:",datasetname)
			#ptm= proc.time()
			if(!is.null(datasetname) && length(grep("uadatasets", datasetname))==0)
			{
				#cat('\nPrefixing with .GlobalEnv\n')
				datasetname <- paste(".GlobalEnv$",datasetname, sep='')
			}
			#print(proc.time() - ptm)
			 #cat(' 3.BSkyValidateDataset : ')
			 #cat(datasetname)
			return(invisible(datasetname))  ##Always return .GlobalEnv$datasetname
			
}

## checks if dataset exists in slice index. Returns +ve index if it does else negative index is returned
BSkyValidateDatasetFromSlice <- function(dataSetSliceIndex)
{
			datasetname <- NULL
			if(is.numeric(dataSetSliceIndex)) ## if numeric
			{
				idx <- dataSetSliceIndex
				totalds = length(uadatasets$lst) 
				if(idx <= totalds && !is.null(uadatasets$lst[idx][[1]])) 	
				{
					datasetname = paste('uadatasets$lst[[',idx,']]',sep='')
				}
			}
			# else ## if char dataset name is given
			# {
				# DatasetIndex <- UAgetIndexOfDataSet(dataSetSliceIndex)
				# if(DatasetIndex > 0)
					# datasetname <- dataSetSliceIndex
			# }
			return(invisible(datasetname))
}

## return col index if its valid otherwise returns -1
BSkyValidateColumn <- function(datasetname, colNameOrIndex)
{
		colIndex <- -1
		if(is.numeric(colNameOrIndex))
		{
			totalcol = eval(parse(text=paste('ncol(',datasetname,')')))
			if(colNameOrIndex <= totalcol)		
				colIndex <- colNameOrIndex
		}
		else
		{
			# cat("\nDS NAME:\n")
			# print(datasetname)
			# cat("\t ColName:")
			#print(colNameOrIndex)
			if(!is.null(colNameOrIndex))
				colIndex <- UAgetIndexOfColInDataSet(datasetname,colNameOrIndex)
		}
		#cat("\nCol Validation Status:",colIndex);
		return(invisible(colIndex))
}

TestFunc <- function(filepath)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("TestFunc: Error in running tet func : ", "filepath :", paste(filepath, collapse = ","),sep="")
	BSkyWarnMsg = paste("TestFunc: Warning in running tet func : ", "filepath :", paste(filepath, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
	
		withCallingHandlers(
		{
			filename <- BSkyGetFilenameOrPath(fullpathfilename=filepath, path=FALSE)
			#cat("\nFilename is:",filename,"\n")

		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution	
	
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
	
	    if(BSkyLocalErrorFound() == TRUE)
    	{
			# cat("Error caught in BSkyFormat \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level test function function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
			# cat("Warning caught in test function \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		# cat("Returning return structure from this top level BSky Foramt function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(obj)))))
}


isUniqueColumns <- function(datasetname)
{

	n <- eval(parse(text=paste('length(names(',datasetname,'))')))
	u <-  eval(parse(text=paste('length(unique(names(',datasetname,')))')))
	if(n == u)
	{
		return (TRUE)
	}
	else
	{
		return (FALSE)
	}
}

## if there is .GlobalEnv$ attached to the dataset name then extract the dataset name from the string (dropping .GlobalEnv$')
## Example: 'datasetname' may look like '.GlobalEnv$Dataset1' or just 'Dataset1'. Return value in both cases will be 'Dataset1'
ExtractDatasetNameFromGlobal <- function(datasetname)
{
		if(is.character(datasetname))
		{
			charcount <- nchar(datasetname)
			if(charcount > 11) # .GlobalEnv$
			{
				if(substr(datasetname, 1,11) == ".GlobalEnv$")
				{
					datasetname = substr(datasetname, 12,charcount)
				}
			}
		}
		return(invisible(datasetname))
}


## backup col attributes of all the cols of a dataset in BSkyblankDSallColAttr
backupAllColAttr <- function(datasetname){

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("backupAllColAttr: Error restoring all col attributes: ", "DatasetnameOrIndex :", datasetname,sep="")
	BSkyWarnMsg = paste("backupAllColAttr: Warning restoring all col attributes: ", "DatasetnameOrIndex :", datasetname,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	BSkyblankDSallColAttr <<- list()
	datasetname <- BSkyValidateDataset(datasetname)
	if(!is.null(datasetname))
	{
		dscolnames <- eval(parse(text=paste('colnames(',datasetname,')',sep='')))
		for(colname in dscolnames) 
		{
			colIndex <- BSkyValidateColumn(datasetname, colname)	
			if(colIndex > 0)
			{
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname)
				# eval(parse(text=paste('BSkyblankDSallColAttr$',colname,' <<- bskyattrs' ,sep='')))
				BSkyblankDSallColAttr <<- append(BSkyblankDSallColAttr, list(bskyattrs))
			}
		}
	}
	BSkyFunctionWrapUp()
}

## restore col attributes of all the col of a dataset in BSkyblankDSallColAttr
restoreAllColAttr <- function(datasetname){
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	# cat("\nRestoring Attr..")
	BSkyErrMsg = paste("restoreAllColAttr: Error restoring all col attributes: ", "DatasetnameOrIndex :", datasetname,sep="")
	BSkyWarnMsg = paste("restoreAllColAttr: Warning restoring all col attributes: ", "DatasetnameOrIndex :", datasetname,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	datasetname <- BSkyValidateDataset(datasetname)
	if(!is.null(datasetname))
	{
		dscolnames <- eval(parse(text=paste('colnames(',datasetname,')',sep='')))
		for(colname in dscolnames) 
		{
			colIndex <- BSkyValidateColumn(datasetname, colname)	
			if(colIndex > 0) 
			{
				bskyattrs <- BSkyblankDSallColAttr[[colIndex]]
				# bskyattrs = eval(parse(text=paste('BSkyblankDSallColAttr$',colname, sep='')))
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)
			}	
		}
	}
	BSkyFunctionWrapUp()
}
