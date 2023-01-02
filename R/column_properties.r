######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAgetDataframeSplitProp
# UAsetDataframeSplitProp
# BSkyRemoveAllSplits
# UAremoveDataframeSplitProp
# UAgetColNames
# UAsetColNames
# UAgetColType
# UAsetColType
# UAgetColDesc
# UAsetColDesc
# UAgetColLevels
# UAsetColLevels
# UAgetColMissing
# UAsetColMissing
# UAgetColMeasure
# UAsetColMeasure
# UAgetColAlign
# UAsetColAlign
# UAgetColLevelSplit
# UAsetColLevelSplit
# UAgetColProperties
# UAsetColProperties
# BSkyMakeColumnFactor
# BSkyMakeMultiColumnFactor
# BSkyMakeColumnString
# BSkyMakeColumnNumeric
# BSkyMakeColumnInteger
#################################################################################################

#############################################################################################################
#Function: UAgetDataframeSplitProp(dataSetNameOrIndex)												
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#																											
#Description:
#		Returns data frame level split property.
#																											
#Example: UAgetDataframeSplitProp("mydataset")																								
#																											
#############################################################################################################
UAgetDataframeSplitProp<- function(dataSetNameOrIndex, isDSValidated=FALSE)
{

	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetDataframeSplitProp: Error getting dataframe split : ", "DataSetName :", dataSetNameOrIndex,sep="")
	#BSkyWarnMsg = paste("UAgetDataframeSplitProp: Warning getting dataframe split : ", "DataSetName :", dataSetNameOrIndex,sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

dfsplitcolindex <- ""	
dfsplitprop <- list()	

	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
		
			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				# if(colIndex < 1){
					# #Error: throw error message. Col not found
				# }	
				#cat('\nDF-SPlit')				
				dfsplit <- eval(parse(text=paste('attr(',datasetname,',"split")',sep='' )))
				if(is.null(dfsplit))#handle NULL
					dfsplit <- c("")
				#print(dfsplit)	
				
				dfsplitcolnames <- eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames")',sep='' ))) 
				if(is.null(dfsplitcolnames))#handle NULL
					dfsplitcolnames <- c("")				
					
				dfsplitcolindex <- eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex")',sep='' ))) 
				if(is.null(dfsplitcolindex))#handle NULL
					dfsplitcolindex <- c("")		
									
				dfsplitprop <- list(DFsplit=dfsplit, DFsplitcolnames=dfsplitcolnames, DFsplitcolindex=dfsplitcolindex)
				#-return(dfsplitprop)
			}
			else
			{
				#cat("\nError: Cannot get Dataframe Split properties. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetDataframeSplitProp: Cannot get Dataframe Split properties. Dataset name not found."," Dataset Name:", datasetname)
				warning("UAgetDataframeSplitProp: Cannot get Dataframe Split properties. Dataset name not found.")
			}
		#BSkyFunctionWrapUp()
return(invisible(dfsplitprop))
}



#############################################################################################################
#Function: UAsetDataframeSplitProp(dataSetNameOrIndex, colNames)										
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNames     				- Name of the columns those are in split       																
#																						
#Description:
#		Set split for some specified columns. Sequence is important here.																	
#																						
#Example: UAsetDataframeSplitProp("mydataset",c("gender","age","patid"))																			
#																						
#############################################################################################################
#uadatasets$lst[[1]][,3] <-factor(uadatasets$lst[[1]][,3])
# attributes(df[,1])<- c(attributes(df[,1]), list(split = c(TRUE)))
# resetAll -> flag will reset all split settings to FALSE as if no split has done. Sets to initial state
#Set dataframe level splits. # index, names list
UAsetDataframeSplitProp	<- function(dataSetNameOrIndex, colNames)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetDataframeSplitProp: Error setting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetDataframeSplitProp: Warning setting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		#cat('\nDatasetName:')
		#cat(datasetname)
			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				# cat(colIndex,"\n:-) ")
				# if(colIndex < 1){
					# #Error: throw error message. Col not found
					# cat("Col not found!")
				# }		
				#cat("\nFirst remove all Splits\n")
				## remove old splits and then add new
				BSkyRemoveAllSplits(datasetname)	
				#cat("\nNow Setting Splits\n")
				##Now Add new cols
				## set dataframe level attributes		
				# attr(uadatasets$lst[[DataSetIndex]],"split")<-c(TRUE)	
				# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnnames")<-colNames
				# colindexes=UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,colNames)
				# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnindex")<-colindexes
				#cat("\nSetting DF Split\n col indexes:\n")
				######  This is for dataset copy outside lst #####
				## set dataframe level attributes		
				# in Following I was gett this error because of double arrow <<-
	#Error in attr(.GlobalEnv$Dataset2, "split") <<- TRUE : 
	#cannot change value of locked binding for '.GlobalEnv'
		#eval(parse(text=paste('attr(',datasetname,',"split") <<- c(TRUE)'))) #<<- #16Jan2016 this line changed to next one
				eval(parse(text=paste('setattr(',datasetname,', "split", c(TRUE) )' )))
				#cat('\nSplit Attr set\n')
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- colNames'))) #<<-16Jan2016 This line replaced by next one
				eval(parse(text=paste('setattr(',datasetname,', "splitcolumnnames", colNames )' )))
				#cat('\nSplitColName Attr set\n')
				colindexes=UAgetIndexsOfColsInDataSet(datasetname,colNames)
				#cat("Col Indexes for split:")
				#print(colindexes)
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- colindexes')))	#<<-16Jan2016 This line replaced with next one
				eval(parse(text=paste('setattr(',datasetname,', "splitcolumnindex", colindexes )' )))				
				#cat("\nDFSplit set\n")
				# print(attr(accidents, "split"))
				##for each col in colNames set col_level split prop, use UAsetColLevelSplit
				for(i in 1:length(colNames))
				{
					#cat("\t",colNames[i]);
					UAsetColLevelSplit(datasetname, colNames[i], colsplit=TRUE)
				}
				#cat('\nFinished setting splits')
#print(attributes(accidents)$split)
			}
			else
			{
				cat("\nError: Cannot set Dataframe Split properties. Dataset name not found\n")
				BSkyErrMsg =paste("UAsetDataframeSplitProp: Cannot set Dataframe Split properties. Dataset name not found."," Dataset Name:", datasetname)
				warning("UAsetDataframeSplitProp: Cannot set Dataframe Split properties. Dataset name not found.")
			}
		BSkyFunctionWrapUp()
		return(invisible())
}	



BSkyRemoveAllSplits<-function(datasetname)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("BSkyRemoveAllSplits: Error removing dataframe split : ", "DataSetName :", datasetname,sep="")
	BSkyWarnMsg = paste("BSkyRemoveAllSplits: Warning removing dataframe split : ", "DataSetName :", datasetname,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	##reset each col in dataset those were taking part in split
	colNames <- UAgetDataframeSplitProp(datasetname)$DFsplitcolnames
	 #cat("\nBSkyRemoveAllSplits: DFsplitcolnames::")
	 #print(colNames)
	 #cat("\nUnsetting Splits if any(remove all splits):")
	if(!(is.null(colNames)) && nchar(colNames) > 0)    #!(is.null(colNames)) || !(is.na(colNames)) || length(colNames) > 0)
	{					
		#cat("\nStarting loop!")
		for(i in 1:length(colNames))
		{
			#cat("\nLoop count : ")
			#cat(i)
			UAsetColLevelSplit(datasetname, colNames[i], colsplit=FALSE)
		}	
	}	
	#cat("\nGetting out of remove all\n")
	BSkyFunctionWrapUp()
	return(invisible())
}

#############################################################################################################
#Function: UAremoveDataframeSplitProp(dataSetNameOrIndex, colNames, removeAllSplits=FALSE)										
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNames     				- Name of the columns those are in split   
#		removeAllSplits    			- TRUE for remove all cols splits. FALSE for remove split from some specified cols.
#																						
#Description:
#		Remove split from some specified columns or all split columns. 																
#																						
#Example: UAremoveDataframeSplitProp("mydataset",c("gender","age","patid"), removeAllSplits=FALSE)																			
#																						
#############################################################################################################
UAremoveDataframeSplitProp	<- function(dataSetNameOrIndex, colNames, removeAllSplits=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAremoveDataframeSplitProp: Error removing dataframe split ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAremoveDataframeSplitProp: Warning removing dataframe split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

			 # cat("Finding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				# cat("Found dataset index:",DataSetIndex,"\n")
				#Error: dataSetName not found
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				# cat(colIndex,"\n:-) ")
				# if(colIndex < 1){
					# #Error: throw error message. Col not found
					# cat("Col not found!")
				# }		
			
				if(removeAllSplits || is.null(colNames) || is.na(colNames) || length(colNames)==0)
				{
					#cat("\nRemoving all splits..")
					BSkyRemoveAllSplits(datasetname)
					# attr(uadatasets$lst[[DataSetIndex]],"split")<-c(FALSE)		
					# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnnames")<-c()
					# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnindex")<-c()
					
					###### Fix this later ### This is for dataset copy outside lst #####
		#eval(parse(text=paste('attr(',datasetname,',"split") <<- c(FALSE)'	)))  #<<- #16Jan2016 this line changed to next one
				eval(parse(text=paste('setattr(x=',datasetname,', name= "split", value= c(FALSE) )' )))
				#cat('\nSplit Attr set\n')
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- c()'))) #<<-16Jan2016 This line replaced by next one
				eval(parse(text=paste('setattr(x=',datasetname,', name= "splitcolumnnames", value= c() )' )))
				#cat('\nSplitColName Attr set\n')
				#colindexes=UAgetIndexsOfColsInDataSet(datasetname,colNames)
				#cat("Col Indexes for split:")
				#print(colindexes)
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- c()')))	#<<-16Jan2016 This line replaced with next one
				eval(parse(text=paste('setattr(x=',datasetname,', name= "splitcolumnindex", value= c() )' )))				
				#cat("\nDFSplit unset\n")
				
				}
				else ## remove some specified cols  which are not taking part in split any more.
				{
					 #cat("\nRemoving splits one by one..")
					##remove few specified col from split list
					for(i in 1:length(colNames))
					{
						UAsetColLevelSplit(datasetname, colNames[i], colsplit=FALSE)
					}	
					# cat("\n#################")
						#Check if all columns are reset. If so then reset dataframe level split prop.
					nosplit=TRUE
					allcolNames <- UAgetColNames(DataSetIndex)
					# print(allcolNames)
					for(i in 1:length(allcolNames))
					{
						if(UAgetColLevelSplit(allcolNames[i], datasetname))
						{
							nosplit=FALSE
							# cat("\nNoSplit FALSE\n")
						}						
					}		
					# cat("\n$$$$$$$$$$$$$$$$$")
					if(nosplit) ## if there is no col with split=TRUE then reset dataframe level split prop
					{
						#cat("\nNoSplit TRUE\n")
						# attr(uadatasets$lst[[DataSetIndex]],"split")<-c(FALSE)		
						# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnnames")<-c()
						# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnindex")<-c()		

						###### Fix this later ### This is for dataset copy outside lst #####
		#eval(parse(text=paste('attr(',datasetname,',"split") <<- c(FALSE)'	)))  #<<- #16Jan2016 this line changed to next one
				eval(parse(text=paste('setattr(x= ',datasetname,', name= "split", value= c(FALSE) )' )))
				#cat('\nSplit Attr set\n')
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- c()'))) #<<-16Jan2016 This line replaced by next one
				eval(parse(text=paste('(x= ',datasetname,', name= "splitcolumnnames", value= c() )' )))
				#cat('\nSplitColName Attr set\n')
				colindexes=UAgetIndexsOfColsInDataSet(datasetname,colNames)
				#cat("Col Indexes for split:")
				#print(colindexes)
		#eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- c()')))	#<<-16Jan2016 This line replaced with next one
				eval(parse(text=paste('setattr(x= ',datasetname,', name= "splitcolumnindex", value= c() )' )))				
				#cat("\nDFSplit unset\n")
					}
					else #change the dataframe level split prop for few cols those are still part of split
					{
						#method1 starts
						newcols <- c()
						oldcols <- UAgetDataframeSplitProp(datasetname)$DFsplitcolnames
						for(i in 1:length(oldcols))# find the rows those are still part of split
						{
							found=FALSE
							for(j in 1:length(colNames))
							{
								if(oldcols[i] == colNames[j])
								{
									found=TRUE
								}
							}
							if(!found)
							{
								newcols <- c( newcols, oldcols[i])
								#cat("adding new cols\n")
							}
						}

						#method1 ends
						
						# #method2 starts
						# newcols <- c()
						# for(i in 1:length(allcolNames))
						# {
							# if(UAgetColLevelSplit(allcolNames[i], dataSetNameOrIndex))
							# {
								# newcols <- list( newcols, allcolNames[i])
							# }						
						# }
						# #method2 ends
						#newcols will have all the cols those are still taking part in split
						# attr(uadatasets$lst[[DataSetIndex]],"split")<-c(TRUE)	
						# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnnames")<-newcols
						# colindexes=UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,newcols)
						# attr(uadatasets$lst[[DataSetIndex]],"splitcolumnindex")<-colindexes		

						###### Fix this later ### This is for dataset copy outside lst #####
						## set dataframe level attributes		
						#eval(parse(text=paste('attr(',datasetname,',"split") <<- c(TRUE)'	)))#<<-16Jan2016 This line replaced  with next one
						eval(parse(text=paste('setattr(x=',datasetname,', name="split", value= c(TRUE) )' )))
						#eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- newcols')))#<<-16Jan2016 This line replaced  with next one
						eval(parse(text=paste('setattr(x=',datasetname,', name="splitcolumnnames", value= c(newcols) )' )))
						colindexes=UAgetIndexsOfColsInDataSet(datasetname,newcols)
						#eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- colindexes')))	#<<-16Jan2016 This line replaced  with next one						
						eval(parse(text=paste('setattr(x=',datasetname,', name="splitcolumnindex", value= c(colindexes) )' )))	
					}
				}
			}
			else
			{
				#cat("\nError: Cannot set Dataframe Split properties. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAremoveDataframeSplitProp: Cannot set Dataframe Split properties. Dataset name not found."," Dataset Name:", datasetname)
				warning("UAremoveDataframeSplitProp: Cannot set Dataframe Split properties. Dataset name not found.")
			}
		BSkyFunctionWrapUp()
		return(invisible())	
}	



#############################################################################################################
#Function: 	UAgetColNames(dataSetNameOrIndex)											
#              																			
#Parameters: 
#		dataSetNameOrIndex			 - dataset name that was given when SPSS file was loaded in memory
#																						
#Description:
#		Return the NAME column entries of SPSS variable view. These entries are col headers in data view.
#																						
#Example: UAgetColNames("mydataset")												
#############################################################################################################
UAgetColNames <- function(dataSetNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColNames: Error getting col names : ", "DataSetName :", dataSetNameOrIndex,sep="")
	#BSkyWarnMsg = paste("UAgetColNames: Warning getting col names : ", "DataSetName :", dataSetNameOrIndex,sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	dataView_Names <- ""	

			#cat("\nPrinting Data View NAMES Col..\n")
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
			
			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
				#cat("\nFound at index..\n")
				#cat(DataSetIndex)
				# dataView_Names <- (attributes(uadatasets$lst[[DataSetIndex]]))$names ## or following
				dataView_Names <- eval(parse(text=paste('colnames(',datasetname,')'))) # colnames(uadatasets$lst[[DataSetIndex]])		
				#cat("\nDisplaying now..\n")
				#cat(dataView_Names)
				#cat("\n*** End ***\n")
			#	return(length(dataView_Names)) #total values in dataView_Names
				if(is.null(dataView_Names))#handle NULL
					dataView_Names <- ""					
				#-return(dataView_Names)
			}
			else
			{
				#cat("\nError: Cannot get col Names. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColNames: Cannot get col Names. Dataset name not found."," Dataset Name:", dataSetNameOrIndex)
				warning("UAgetColNames: Cannot get col Names. Dataset name not found.")
			}			
	

		#BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(dataView_Names)) #return(BSkyReturnStructure(dataView_Names))

}




#############################################################################################################
#Function: 	UAgetColNames(dataSetNameOrIndex)											
#              																			
#Parameters: 
#		dataSetNameOrIndex			 - dataset name that was given when SPSS file was loaded in memory
#																						
#Description:
#		Return the NAME column entries of SPSS variable view. These entries are col headers in data view.
#																						
#Example: UAgetColNames("mydataset")												
#############################################################################################################
UAsetColNames <- function(dataSetNameOrIndex, colNameOrIndex, newName)#Name
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColNames: Error setting col names : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColNames: Warning setting col names : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
			#cat("\nPrinting Data View NAMES Col..\n")
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			
			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)		
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				if(colIndex > 0){	
					#cat("\nadding col name")			
					eval(parse(text=paste('colnames(',datasetname,')[',colIndex,'] <- newName',sep=''))) ## names(uadatasets$lst[[DataSetIndex]])[colIndex] <- newName	#. <<- to <-
					#cat("\nadded col name")
					## Also if missing exisits. Change name there also. $misvals$accid
					#cazing prbl names(attributes(uadatasets$lst[[DataSetIndex]])$misvals)[colIndex] <- newName
 					#cat("\nadded col misval")
				}
				else
				{
					#cat("\nError: Cannot set col Name. Column name or index not found\n")
					BSkyErrMsg =paste("UAsetColNames: Cannot set col Name. Column name or index not found."," Dataset Name:", datasetname)
					warning("UAsetColNames: Cannot set col Name. Column name or index not found.")
				}
			
			}
			else
			{
				#cat("\nError: Cannot set col Names. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColNames: Cannot set col Names. Dataset name not found."," Dataset Name:", datasetname)
				warning("UAsetColNames: Cannot set col Names. Dataset name not found.")
			}			

		BSkyFunctionWrapUp()
		return(invisible())	
}



#############################################################################################################
#Function: UAgetColType(dataSetNameOrIndex,colNameOrIndex)										
#              																			
#Parameters:   
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory
#		colNameOrIndex     			- Name/index of the column whose Type is needed
#																						
#Description:	
#		Returns TYPE column entry of SPSS variable view for specific column.
#																						
#Example: UAgetColType("mydataset", "age")													
#############################################################################################################
UAgetColType <- function(dataSetNameOrIndex,colNameOrIndex, as.class=FALSE, isDSValidated=FALSE)#Type
{

	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColType: Error getting col type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColType: Warning getting col type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	colType<-c()
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
			
			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				if(colIndex > 0){
					colType<-c()
					if(as.class)
					{
					colType <- eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])',sep='')))
					#cat("\nCol Class:")
					}
					else
					{			
					colType <- eval(parse(text=paste('typeof(',datasetname,'[[',colIndex,']])',sep='')))
					#cat("\nCol Type:")
					}		
					#cat("\nColType:")
					#cat(colType)
					#cat("\n")
				}		
				else
				{
					# cat("\nError: Cannot get col type. Col not found\n")
					BSkyErrMsg =paste("UAgetColType: Cannot get col type. Col not found."," column Name:", colNameOrIndex)
					warning("UAgetColType: Cannot get col type. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot get col type. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColType: Cannot get col type. Dataset name not found"," Dataset Name:", datasetname)
				warning("UAgetColType: Cannot get col type. Dataset name or index not found")
			}			
		#BSkyFunctionWrapUp()
	if(is.null(colType))#handle NULL
		colType <- ""	
	return(invisible(colType))
}



#############################################################################################################
#Function: UAgetColType(dataSetNameOrIndex,colNameOrIndex)										
#              																			
#Parameters:   
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory
#		colNameOrIndex     			- Name/index of the column whose Type is needed
#																						
#Description:	
#		Returns TYPE column entry of SPSS variable view for specific column.
#																						
#Example: UAgetColType("mydataset", "age")													
#############################################################################################################
UAsetColType <- function(dataSetNameOrIndex,colNameOrIndex, newType) #Type
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColType: Error setting col type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColType: Warning setting col type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
		colType<-c()
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			
			if(!is.null(datasetname) )
			{		
				#Error: Dataset not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				if(colIndex > 0){
					#- colType<-c()
					#if(as.class)
					#{  
						if(newType == "numeric" || newType == "integer" || newType == "character"|| newType == "logical")
							eval(parse(text=paste('class(',datasetname,'[[',colIndex,']]) <- newType',sep=''))) #. <<- to <-
					#}
					#else
					#{			
						#typeof(uadatasets$lst[[DataSetIndex]][[colIndex]]) <- newType
					#}		

				}		
				else
				{
					# cat("\nError: Cannot set col type. Col not found\n")
					BSkyErrMsg =paste("UAsetColType: Cannot set col type. Col not found"," Dataset Name:", datasetname)
					warning("UAsetColType: Cannot set col type. Col not found")
				}
			}
			else
			{
				# cat("\nError: Cannot set col type. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColType: Cannot set col type. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAsetColType: Cannot set col type. Dataset name or index not found")
			}			
		BSkyFunctionWrapUp()
		return(invisible())
}



#############################################################################################################
#Function: UAgetColDesc(dataSetNameOrIndex, colNameOrIndex)										
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Missing values are needed      																
#																						
#Description:	
#		Returns attribute "coldesc" for specific column.																		
#																						
#Example: UAgetColDesc("mydataset","agecat")																			
#############################################################################################################
#get col description same as UAgetSPSSVariableView_Lable but uses extra col level attribute "coldesc" to get result
UAgetColDesc <- function(dataSetNameOrIndex, colNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColDesc: Error getting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColDesc: Warning getting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	colDesc <- ""	
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
		
			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				#colDesc <- ""	
				if(colIndex > 0){
					
					if(is.null(eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"coldesc")',sep='')))))
						eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"coldesc") <- ""',sep=''))) 

					colDesc <- eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"coldesc")',sep='')))##coldesc is not standard BSky attribute

						#colDesc <- attr(uadatasets$lst[[DataSetIndex]], "variable.labels")[[colIndex]] 

					#if(is.null(colDesc))#handle NULL
						#colDesc <- ""	

						#-return(colDesc)
				}	
				else
				{
					# cat("\nError: Cannot get Col Description. Col not found\n")
					BSkyErrMsg =paste("UAgetColDesc: Cannot get Col Description. Col not found"," Dataset Name:", datasetname)
					warning("UAgetColDesc: Cannot get Col Description. Col not found")
				}
			}
			else
			{
				# cat("\nError: Cannot get Col Description. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColDesc: Cannot get Col Description. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAgetColDesc: Cannot get Col Description. Dataset name or index not found")
			}
		#BSkyFunctionWrapUp()
return(invisible(colDesc))
}

#############################################################################################################
#Function: UAsetColDesc(dataSetNameOrIndex, colNameOrIndex, newLabel)										
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Missing values are needed  
#		newLabel					- New label in place of old one    																
#																						
#Description:	
#		Sets attribute "coldesc" for specific column.																		
#																						
#Example: UAsetColDesc("mydataset","agecat", "Age Cat")																			
#############################################################################################################
#get col description same as UAgetSPSSVariableView_Lable but uses extra col level attribute "coldesc" to get result
UAsetColDesc <- function(dataSetNameOrIndex, colNameOrIndex, newLabel)#Label
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColDesc: Error setting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColDesc: Warning setting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName) 

				if(colIndex > 0){
					#attr(uadatasets$lst[[DataSetIndex]][,colIndex],"coldesc") <- newLabel
					#17Jul2015 eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"coldesc") <<- newLabel',sep=''))) #<<-
					eval(parse(text=paste('setattr(x=',datasetname,'[,',colIndex,'], name= "coldesc", value= newLabel)' ,sep='')))#17Jul2015 
					#cat("\ncoldesc Set.")
					#attr(uadatasets$lst[[DataSetIndex]], "variable.labels")[[colIndex]] <- newLabel
					eval(parse(text=paste('attr(',datasetname,',"variable.labels")[[colIndex]] <- newLabel',sep=''))) #<<- #. <<- to <-
					#eval(parse(text=paste('setattr(',datasetname,', "variable.labels", newLabel)' ,sep='')))#17Jul2015 
					#cat("\nvariable.labels Set.")
				}	
				else
				{
					# cat("\nError: Cannot set Col Description. Col not found\n")
					BSkyErrMsg =paste("UAsetColDesc: Cannot set Col Description. Col not found"," Column Name:", colNameOrIndex)
					warning("UAsetColDesc: Cannot set Col Description. Col not found")
				}
			}
			else
			{
				# cat("\nError: Cannot set Col Description. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColDesc: Cannot set Col Description. Dataset name not found"," Dataset Name:", datasetname)
				warning("UAsetColDesc: Cannot set Col Description. Dataset name or index not found")
			}


		BSkyFunctionWrapUp()
		return(invisible())
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		#return(BSkyReturnStructure()) #
}

#############################################################################################################
#Function: UAgetColLevels(dataSetNameOrIndex, colNameOrIndex)									
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose factor values are needed            																
#																						
#Description:	
#		Returns the level (male, female) entry from VALUES column of SPSS variable view	for a specific column
#																						
#Example: UAgetColLevels("mydataset","gender")																			
#############################################################################################################
UAgetColLevels <- function(dataSetNameOrIndex, colNameOrIndex,isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColLevels: Error getting col levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColLevels: Warning getting col levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	#cat("\n0.Levels are:")
	colLevels <- ""	
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1

			if(!is.null(datasetname))
			{		
			#cat("\n1.Levels are:")
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
#cat("\n2.Levels are:")	
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
					# colLevels <- eval(parse(text=paste('levels(',datasetname,'[,',colIndex,'])', sep='')))
					# Line above is not returning levesl for new dataset generated by Aggregate 
					#so by chaning syntax to following should work
					colLevels <- eval(parse(text=paste('levels(',datasetname,'[[',colIndex,']])', sep='')))
					#cat("\n3.Levels are:")
					if(is.null(colLevels))#handle NULL. for non factor col the levels are NULL
						colLevels <- ""		
					else if( identical(colLevels, character(0)))#03Aug2016 else-if added. For factor cols with no levels, levels=character(0). This is for new factor col added to DS
					{
						colLevels <- "<NA>"
					}					
					#-return(colLevels)
					#cat("\nLevels are:")
					 #print(colLevels)
				}			
				else
				{
					# cat("\nError: Cannot get col levels. Col not found\n")
					BSkyErrMsg =paste("UAgetColLevels: Cannot get col levels. Col not found"," Column Name:", colNameOrIndex)
					warning("UAgetColLevels: Cannot get col levels. Col not found")
					#-return("")
				}

			}
			else
			{
				# cat("\nError: Cannot get col levels. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColLevels: Cannot get col levels. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAgetColLevels: Cannot get col levels. Dataset name or index not found")
			}
		#BSkyFunctionWrapUp()
		return(invisible(colLevels))
}



#############################################################################################################
#Function: UAgetColLevels(dataSetNameOrIndex, colNameOrIndex, newLevels)									
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose factor values are needed 
#		newLevels					- New Levels for factor column           																
#																						
#Description:	
#		Returns the level (male, female) entry from VALUES column of SPSS variable view	for a specific column
#																						
#Example: UAgetColLevels("mydataset","gender", c("what", "who","when"))																			
#############################################################################################################
UAsetColLevels <- function(dataSetNameOrIndex, colNameOrIndex, newLevels)#Values
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColLevels: Error setting col levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColLevels: Warning setting col levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
#cat("\n1.Setting Levels.")		
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
				#cat("\n2Setting Levels.")
					eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,']) <- c(attributes(',datasetname,'[,',colIndex,']), list(levelLabels = UAgetColLevels("',datasetname,'", ',colIndex,')))',sep='')))#. <<- to <-
					# levels(uadatasets$lst[[DataSetIndex]][[colIndex]]) <- newLevels
					#cat("\n3.Setting Levels.")
					eval(parse(text=paste('levels(',datasetname,'[[',colIndex,']]) <- newLevels',sep='')))#. <<- to <-
					#cat("\nLvl Changed")
				}			
				else
				{
					# cat("\nError: Cannot set col levels. Col not found\n")
					BSkyErrMsg =paste("UAsetColLevels: Cannot set col levels. Col not found"," Column Name:", colNameOrIndex)
					warning("UAsetColLevels: Cannot set col levels. Col not found")
				}
#cat("\n4Setting Levels.")
			}
			else
			{
				# cat("\nError: Cannot set col levels. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColLevels: Cannot set col levels. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAsetColLevels: Cannot set col levels. Dataset name or index not found")
			}
		BSkyFunctionWrapUp()
		return(invisible())	
}



#############################################################################################################
#Function: UAgetColMissing(dataSetNameOrIndex, colNameOrIndex)									
#              																			
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Missing values are needed             																
#																						
#Description:			
#		Returns the "missingvalues" attribute for a specific column																
#																						
#Example: UAgetColMissing("mydataset","gender")																			
#############################################################################################################
UAgetColMissing <- function(dataSetNameOrIndex, colNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColMissing: Error getting col missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColMissing: Warning getting col missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	misvals <- ""
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1

			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				#cat("Col idx", colIndex)
				if(colIndex > 0){
					#if(attr(uadatasets$lst[[DataSetIndex]][,colIndex],"usermissing"))# returns TRUE if there are missing vals
					#{
						#misvals <- attr(uadatasets$lst[[DataSetIndex]][,colIndex],"missingvalues")
					#}
					#else
					#{
						#misvals<- c("")
					#}
					## misvalues <- attr(uadatasets$lst[[DataSetIndex]],"missings")[[colIndex]]$value

					##Checking if attr exists. Otherwise create it
					#####if(is.null(eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"misvals")',sep=''))) ))
						#####eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"misvals") <- ""',sep=''))) 

					coluname = eval(parse(text=paste('colnames(',datasetname,')[',colIndex,']')))
					currentval = eval(parse(text=paste('attr(',datasetname,', "misvals_',coluname,'")',sep='' )))
					if(is.null(currentval) || currentval=="")
					{
						colmisatt <- eval(parse(text=paste('list(type="none", value="")')))					
						eval(parse(text=paste('setattr(x=',datasetname,', name= "misvals_',coluname,'",value= colmisatt )',sep='' )))#attr for Dataset$colname
						misvals <- colmisatt
					}
					else
					{
						misvals <- eval(parse(text=paste('attr(',datasetname,',"misvals_',coluname,'")',sep=''))) 
					}
					
					#if ("misvals" %in% names(attributes(uadatasets$lst[[DataSetIndex]]))) ##Checking if attribute exisits
					# colexists <- names(uadatasets$lst[[DataSetIndex]][colIndex]) %in% names(attr(uadatasets$lst[[DataSetIndex]],"misvals"))
					#####colexists <- eval(parse(text=paste('names(',datasetname,'[',colIndex,']) %in% names(attr(',datasetname,',"misvals"))',sep='')))
					#####if (colexists)##Checking if colname exists in attribute
					#####{ 
						 #####cat("\nMissing Col Found...\n")
						#####misvals <- eval(parse(text=paste('attr(',datasetname,',"misvals")[',colIndex,']',sep=''))) # $type and $value
						
						
						 #####print(misvals)
					#if(is.null(misvals))#handle NULL
						#misvals <- ""	
						#-return(misvals)
					#####}
					#####else
					#####{ 
						#-return("")
						#cat("AA")
					#####}
					#cat("AAA")
				}			
				else
				{
					# cat("\nError: Cannot get col Missing. Col not found\n")
					BSkyErrMsg =paste("UAgetColMissing: Cannot get col Missing. Col not found"," Column Name:", colNameOrIndex)
					warning("UAgetColMissing: Cannot get col Missing. Col not found")
				}
			}
			else
			{
				# cat("\nError: Cannot get col Missing. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColMissing:  Cannot get col Missing. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAgetColMissing:  Cannot get col Missing. Dataset name or index not found")
			}			
		#BSkyFunctionWrapUp()
		return(invisible(misvals))
}


#############################################################################################################
#Function: UAsetColMissing(dataSetNameOrIndex, colNameOrIndex,misvals)									
#              																			
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Missing values need to be set      
#		misvals						- Missing values for this column       																
#																						
#Description:			
#		Sets missing values for specified column in a dataset which is already in UA memory space																
#																						
#Example: UAsetColMissing("mydataset","age", c(100,200,300))																			
#																						
#############################################################################################################
UAsetColMissing <- function(dataSetNameOrIndex, colNameOrIndex, misvals, mistype)#Missing
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColMissing: Error setting col missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColMissing: Warning setting col missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))	
			{		
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
					#Error: throw error message. Col not found
							#
					#if(!(is.null(misvals)) && length(misvals)>0)
					#{
						#attr(uadatasets$lst[[DataSetIndex]][,colIndex],"usermissing")<-c(TRUE)
						#attr(uadatasets$lst[[DataSetIndex]][,colIndex],"missingvalues")<- misvals
					#}
					#else
					#{
						#attr(uadatasets$lst[[DataSetIndex]][,colIndex],"usermissing")<-c(TRUE)
						#attr(uadatasets$lst[[DataSetIndex]][,colIndex],"missingvalues")<- c()
					#}
					#cat("\n Set missing.. ")
					#print(misvals)
					###check to see that mistype must be one of - "none", "three", "range+1" ###
					colexists <- eval(parse(text=paste('names(',datasetname,'[',colIndex,']) %in% names(attr(',datasetname,',"misvals"))',sep='')))
					if (colexists)##Checking if colname exists in attribute
					{
						if(mistype!="none")
							eval(parse(text=paste('attr(',datasetname,',"misvals")[[',colIndex,']]$value <- misvals', sep='')))#. <<- to <-
						else
							eval(parse(text=paste('attr(',datasetname,',"misvals")[[',colIndex,']]$value <- c("")',  sep='')))#. <<- to <-
							
						# attr(datasetname,"misvals")[[colIndex]]$type <- mistype
						eval(parse(text=paste('attr(',datasetname,',"misvals")[[',colIndex,']]$type <- mistype', sep='')))#. <<- to <-
						# return(misvalues)
						#cat(" done missing..")
						#print(attr(acc, "misvals")[[colIndex]]$value)
					}
					#cat(" missing exit..")
				}
				else
				{
					# cat("\nError: Cannot set col Missing. Col not found\n")
					BSkyErrMsg =paste("UAsetColMissing:  Cannot set col Missing. Col not found"," Colname Name:", colNameOrIndex)
					warning("UAsetColMissing:  Cannot set col Missing. Col not found")
				}
			}
			else
			{
				# cat("\nError: Cannot set col Missing. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColMissing: Cannot set col Missing. Dataset name or index not found"," Dataset Name:", datasetname)
				warning("UAsetColMissing: Cannot set col Missing. Dataset name or index not found")
			}
		BSkyFunctionWrapUp()
		return(invisible())
}



#############################################################################################################
#Function: UAgetColMeasure(dataSetNameOrIndex, colNameOrIndex)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Measure value is needed           																
#																						
#Description:	
#		Returns "factor" if the specified col is factor type																		
#																						
#Example: UAgetColMeasure("mydataset","gender")																			
#############################################################################################################
UAgetColMeasure <- function(dataSetNameOrIndex, colNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColMeasure: Error getting col measure : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColMeasure: Warning getting col measure : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	colMeasure <- c("")
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
			# cat("Get Mesure\n")
			if(!is.null(datasetname))	
			{		
				#Error : dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				
				if(colIndex > 0){
				
					isfactor = eval(parse(text=paste('is.factor(',datasetname,'[[',colIndex,']])',sep=''))) 
					isordered = eval(parse(text=paste('is.ordered(',datasetname,'[[',colIndex,']])',sep=''))) 
					# cat("\nFactor = ", isfactor, "\t Ordered = ", isordered)
					if( isfactor  ) # it can be nominal or ordinal
					{
						colMeasure <- c("factor") # "Nominal"
						if(isordered)
							colMeasure <- c("ordinal") # "Ordinal"
					}
					else
					{
						colMeasure <- c("scale") #scale
					}
					#-return(colMeasure)
				}	
				else
				{
					# cat("\nError: Cannot get col Measure. Col not found\n")
					BSkyErrMsg =paste("UAgetColMeasure: Cannot get col Measure. Col not found"," Dataset Name:", colNameOrIndex)
					warning("UAgetColMeasure: Cannot get col Measure. Col not found")
				}
			

			}
			else
			{
				# cat("\nError: Cannot get col Measure. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColMeasure: Cannot get col Measure. Dataset name not found"," Dataset Name:", datasetname)
				warning("UAgetColMeasure: Cannot get col Measure. Dataset name or index not found")
			}			


		#BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(colMeasure)) #
}


#incomplete
#############################################################################################################
#Function: UAgetColMeasure(dataSetNameOrIndex, colNameOrIndex)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose Measure value is needed           																
#																						
#Description:	
#		Returns "factor" if the specified col is factor type																		
#																						
#Example: UAgetColMeasure("mydataset","gender")																			
#############################################################################################################
UAsetColMeasure <- function(dataSetNameOrIndex, colNameOrIndex, newMeasure, newOrder=c()) #Measure
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColMeasure: Error setting col measure : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColMeasure: Warning setting col measure : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
				colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				# cat("\nNew Measure:")
				# print(newMeasure)
				if(colIndex > 0)
				{
					#Added by Aaron 06/25/2020, added string, we are not doing anything for string, added condition to avoid the message UAsetColMeasure: Wrong Measure Value. getting printed
					## 08Jul2022 Anil: I had to add the "Date" for the same reason(as above).
					#if(newMeasure == "Scale" || newMeasure == "Nominal" || newMeasure == "Ordinal")
					if(newMeasure == "Scale" || newMeasure == "Nominal" || newMeasure == "Ordinal" || newMeasure =="String" || newMeasure == "Date")
					{
						if(newMeasure == "Ordinal")##what if newOrder is empty. From C# always send newOrder
						{
							if(length(newOrder)>0)
								eval(parse(text=paste(datasetname,'[,',colIndex,'] <- ordered(',datasetname,'[,',colIndex,'], levels= newOrder)',sep='')))#pass new order#. <<- to <-
							else
								eval(parse(text=paste(datasetname,'[,',colIndex,'] <- as.ordered(',datasetname,'[,',colIndex,'])',sep='')))#. <<- to <-
						}
						else if(newMeasure == "Nominal")
						{
							eval(parse(text=paste(datasetname,'[,',colIndex,'] <- factor(',datasetname,'[,',colIndex,'])', sep='')))#. <<- to <-
						}
						else
						{
							#cat("\nFind class of col..")
							#UAsetColLevels(DataSetIndex, colIndex, newLevels=c(""))
							colclass <- eval(parse(text=paste('class(',datasetname,'[,',colIndex,'])')))
							#cat("\nCol class ")
							#cat(colclass)
							if(!("POSIXct" %in% colclass || "POSIXlt" %in% colclass || "Date" %in% colclass || "character" %in% colclass))
							{
								##08Jul2022  we should not be converting the class of the col.
							#eval(parse(text=paste(datasetname,'[,',colIndex,'] <- as.numeric(',datasetname,'[,',colIndex,'])', sep='')))#. <<- to <-
							}
						}
					}
					else
					{
						# cat("\nWrong Measure Value.\n")
						BSkyErrMsg =paste("UAsetColMeasure: Wrong Measure Value."," Dataset Name:", dataSetNameOrIndex)
						warning("UAsetColMeasure: Wrong Measure Value.")
					}
					if( eval(parse(text=paste('is.factor(',datasetname,'[[',colIndex,']])',sep=''))) )
					{
						colMeasure <- c("factor") # "Nominal"
					}
					# Aaron Commented below 06/25/2020
					else if( eval(parse(text=paste('is.ordered(',datasetname,'[[',colIndex,']])',sep=''))) )
					{
						colMeasure <- c("ordered") # "Nominal"
					}
					
					# Aaron Commented below 06/25/2020
					# else
					# {
						# colMeasure <- c("ordered") #Ordinal
					# }

				}	
				else
				{
					 cat("\nError: Cannot set col Measure. Col not found\n")
					BSkyErrMsg =paste("UAsetColMeasure: Cannot set col Measure. Col not found."," Column Name:", colNameOrIndex)
					warning("UAsetColMeasure: Cannot set col Measure. Col not found.")
				}
			

			}
			else
			{
				# cat("\nError: Cannot set col Measure. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColMeasure: Cannot set col Measure. Dataset name not found."," Dataset Name:", datasetname)
				warning("UAsetColMeasure: Cannot set col Measure. Dataset name or index not found.")
			}			
		BSkyFunctionWrapUp()
		return(invisible())
}

#############################################################################################################
#Function: UAgetColAlign(colNameOrIndex,dataSetNameOrIndex)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose col level split is needed         																
#																						
#Description:	
#		Returns column level split flag.																		
#																						
#Example: UAgetColAlign("gender", "mydataset")		
#############################################################################################################
UAgetColAlign <- function(dataSetNameOrIndex, colNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColAlign: Error getting Col Alignment : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColAlign: Warning getting Col Alignment : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	colalign <- "Left"

#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1

			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0)
				{
					colalign <- eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"align")',sep='')))
					if(is.null(colalign))#handle NULL
						colalign <- "Left"	 # set default
					#-return(colalign)
				}			
				else
				{
					# cat("\nError: Cannot get col Align. Col not found\n")
					BSkyErrMsg =paste("UAgetColAlign: Cannot get col Align. Col not found."," Col Name:", colNameOrIndex)
					warning("UAgetColAlign: Cannot get col Align. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot get col Align. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColAlign: Cannot get col Align. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAgetColAlign: Cannot get col Align. Dataset name or index not found.")
			}			
		#cat("\nAlign:-",colalign)
		#BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n") 
		return(invisible(colalign)) #
}



#############################################################################################################
#Function: UAsetColAlign(colNameOrIndex,dataSetNameOrIndex)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose col level split is needed         																
#																						
#Description:	
#		Returns column level split flag.																		
#																						
#Example: UAsetColAlign("gender", "mydataset")		
#############################################################################################################
UAsetColAlign <- function(dataSetNameOrIndex, colNameOrIndex, colalign)#Alignment
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColAlign: Error setting col alignment : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColAlign: Warning etting col alignment : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
					if(colalign == "Left" || colalign == "Center" || colalign == "Right")
					{
						#cat("\nAlign Setting to:",colalign)
						eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"align") <- colalign',sep='')))#. <<- to <-
						#cat("\nSuccess Align!")
						#print(attr(acc[,colIndex], "align"))
					}
					else
					{
						eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"align") <- "Left"',sep='')))#. <<- to <-
						#cat("\nError: Cannot set col Align. Wrong value for Align\n")
					}
				}			
				else
				{
					# cat("\nError: Cannot set col Align. Col not found\n")
					BSkyErrMsg =paste("UAsetColAlign: Cannot set col Align. Col not found."," Col Name:", colNameOrIndex)
					warning("UAsetColAlign: Cannot set col Align. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot set col Align. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColAlign: Cannot set col Align. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetColAlign: Cannot set col Align. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()
		return(invisible())
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		#return(BSkyReturnStructure()) #
}


#############################################################################################################
#Function: UAgetColLevelSplit(colNameOrIndex,dataSetNameOrIndex)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose col level split is needed         																
#																						
#Description:	
#		Returns column level split flag.																		
#																						
#Example: UAgetColLevelSplit("gender", "mydataset")		
#############################################################################################################
UAgetColLevelSplit <- function(colNameOrIndex, dataSetNameOrIndex, isDSValidated=FALSE)
{
	#BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	#BSkyErrMsg = paste("UAgetColLevelSplit: Error getting col level split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyWarnMsg = paste("UAgetColLevelSplit: Warning getting col level split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	colSplit <- ""	
#datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1

			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
					colSplit <- eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"split")',sep='')))
					if(is.null(colSplit))#handle NULL
						colSplit <- ""						
					#-return(colSplit)
				}			
				else
				{
					# cat("\nError: Cannot get col level split. Col not found\n")
					BSkyErrMsg =paste("UAgetColLevelSplit:Cannot get col level split. Col not found."," Col Name:", colNameOrIndex)
					warning("UAgetColLevelSplit:Cannot get col level split. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot get col level split. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetColLevelSplit:Cannot get col level split. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAgetColLevelSplit:Cannot get col level split. Dataset name or index not found.")
			}

	
		#BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(colSplit))
}


#Set Col level split: 
UAsetColLevelSplit <- function(dataSetNameOrIndex, colNameOrIndex, colsplit=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColLevelSplit: Error setting col level split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColLevelSplit: Warning setting col level split : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				# colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				if(colIndex > 0){
					# attr(datasetname[,colIndex],"split") <- colsplit
					 #cat("\nSplit Col Index:",colIndex,"\n")
					###### ## for datasets copy outside lst #######
		#eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"split") <<- ',colsplit, sep='' )))#16Jan2016 This line replaced by next one
					eval(parse(text=paste('setattr(x=',datasetname,'[,',colIndex,'], name= "split", value= colsplit )' ,sep='')))
					 #cat("\n Split Done:",colIndex,"\n")
					# return(colSplit)
				}			
				else
				{
					# cat("\nError: Cannot set col level Split. Col not found\n")
					BSkyErrMsg =paste("UAsetColLevelSplit: Cannot set col level Split. Col not found."," Column Name:", colNameOrIndex)
					warning("UAsetColLevelSplit: Cannot set col level Split. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot set col level Split. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColLevelSplit: Cannot set col level Split. Dataset name  not found."," Dataset Name:", datasetname)
				warning("UAsetColLevelSplit: Cannot set col level Split. Dataset name  not found.")
			}

		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		#return(BSkyReturnStructure()) 
		return(invisible())
}


#############################################################################################################
#Function: UAgetColProperties(dataSetNameOrIndex, colNameOrIndex)										
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose propeties are needed       																
#		isDSValidated				- if Dataset is validated then dateSetNameOrIndex will have this format : .GlobalEnv$Dataset1. This is must else app may crash.																				
#Description:
#		Returns all column properties of a specific column																		
#																						
#Example: UAgetColProperties("mydataset","gender")																			
#																						
#############################################################################################################
UAgetColProperties <- function(dataSetNameOrIndex, colNameOrIndex, asClass=TRUE, isDSValidated=FALSE)
{
#BSkyFunctionInit()
#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
#BSkyErrMsg = paste("UAgetColProperties: Error getting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
#BSkyWarnMsg = paste("UAgetColProperties: Warning getting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	isdesign = FALSE
	isdataframe = FALSE
	colProperties <- list()
	if(!isDSValidated)
		datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	else
		datasetname <- dataSetNameOrIndex #This must have format like .GlobalEnv$Dataset1
	# cat("DS index:",datasetname)	
			if(!is.null(datasetname))
			{		
				colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)		
				# cat("Col index:",colIndex)	

				#Error: dataSetName and colname not found
				if(colIndex > 0)
				{
					#DoE dataset returns all colnames as once so we need something different here
					# isdataframe = eval(parse(text=paste('"data.frame" %in% c(class(',datasetname,'))',sep='')))
					# isdesign = eval(parse(text=paste('"design" %in% c(class(',datasetname,'))',sep=''))) # for DoE
					# if(isdesign && isdataframe){
					# 	colName <- eval(parse(text=paste('colnames(',datasetname,')[',colIndex,']',sep='')))
					# }
					# else
					# { 	#for non-design data frame
					# 	colName <- eval(parse(text=paste('colnames(',datasetname,'[',colIndex,'])',sep='')))
					# }

					## following should work for bothe "design" as well as "data.frame" class of dataset
					#find col name
					#ptm= proc.time()
					colName <- eval(parse(text=paste('colnames(',datasetname,')[',colIndex,']',sep='')))
					#cat("\nName:",colName)
					#print(proc.time() - ptm)

					#find col type
					#ptm= proc.time()
					colType <- UAgetColType(datasetname, colNameOrIndex, as.class=FALSE,isDSValidated=isDSValidated)
					#cat("\nType:",colType)
					#print(proc.time() - ptm)

					#find col class
					#ptm= proc.time()					
					colClass <- UAgetColType(datasetname, colNameOrIndex, as.class=TRUE, isDSValidated=isDSValidated)
					#cat("\nClass:",colType)
					#print(proc.time() - ptm)
					
					#find col label
					#ptm= proc.time()
					colLabel <- UAgetColDesc(datasetname, colNameOrIndex, isDSValidated=isDSValidated)
					#cat("\nLabel:",colLabel)
					#print(proc.time() - ptm)

					#find col values, level part
					#ptm= proc.time()
					colValues_level <- UAgetColLevels(datasetname, colNameOrIndex,isDSValidated=isDSValidated)
					#cat("\nLevels:",colValues_level)
					#print(proc.time() - ptm)
					
					#find col missing
					#ptm= proc.time()
					# cat("\nFetching Missing")
					# Not
					missings <- UAgetColMissing(datasetname, colNameOrIndex,isDSValidated=isDSValidated)
					# print("\nFetched Missing:-\n")
					# print(missings)
					if(!(is.null(missings) || missings==""))
					{
						#colname <- names(acc[colNameOrIndex])
						#colMissing <- eval(parse(text=paste('missings$',colName,'$value',sep='')))
						#colMissingType <- eval(parse(text=paste('missings$',colName,'$type',sep='')))
						
						
						colMissingType <- eval(parse(text=paste('missings$type',sep='')))
						if(missings$type == "none")
						{
							colMissing <- NULL
						}
						else
						{
							colMissing <- eval(parse(text=paste('missings$value',sep='')))
						}						
						
						
					
						if(is.null(colMissing)) # trying not to send NULL to UI layer
							colMissing = ""		
						if(is.null(colMissingType))#handle NULL
							colMissingType <- "none"				
						# print("\nMissing:",missings$value,"\t Type:",missings$type,"\n")
						# print(missings)
					}
					else
					{
						colMissing <- c("")
						colMissingType <- "none"
						# print("\nDefaults, Missing:",colMissing,"\t Type:",colMissingType)
					}
					#cat("\nMissing:")
					# print(colMissing)
					#print(proc.time() - ptm)
					
					#find col measure
					#ptm= proc.time()
					colMeasure <- UAgetColMeasure(datasetname, colNameOrIndex,isDSValidated=isDSValidated)
					#cat("\nMeasure:",colMeasure)
					#print(proc.time() - ptm)
					
					#find col align
					#ptm= proc.time()
					colAlign <- UAgetColAlign(datasetname, colNameOrIndex,isDSValidated=isDSValidated)
					#cat("\nCol Align:",colAlign)
					#print(proc.time() - ptm)
					
					#find col level split
					#ptm= proc.time()
					colSplit <- UAgetColLevelSplit(colNameOrIndex, datasetname,isDSValidated=isDSValidated)
					#cat("\nCol Split:",colSplit)
					#print(proc.time() - ptm)
					
					##################    some more prop for variable grid   ##################

					#find col level width
					#ptm= proc.time()
					colWidth <- eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$width',sep='')))   
					#cat("\nCol Width:",colWidth)
					#print(proc.time() - ptm)
					
					#find col level decimals
					#ptm= proc.time()
					colDecimals <- eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$decimals',sep='')))   
					#cat("\nCol Decimals:",colDecimals)
					#print(proc.time() - ptm)
					
					#find col level columns
					#ptm= proc.time()
					colColumns <- eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$columns',sep=''))) 
					#cat("\nCol Columns:",colColumns)															
					#print(proc.time() - ptm)
					
					#find col level role
					#ptm= proc.time()
					colRole <- eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"role")',sep='')))
					
					colDateFormat = eval(parse(text=paste('attr(',datasetname,'[,',colIndex,'],"DateFormat")',sep='')))
					#cat("\nCol Role:",colRole)					
					#print(proc.time() - ptm)
					
					#cat("\n\n")
					#Create two col
					##propCol1 <- c( "Name", "Type", "Label", "Factor", "Level", "Missing", "Align", "Measure" )
					##propCol2 <- c(colName, colType, colLabel, list(colValues_factor), list(colValues_level), colMissing#, colMeasure)

					#Create an object with two col #removed -> Factor=colValues_factor,
					#ptm= proc.time()
					#colProperties <- list()#Name=colName, Type=colType, Label=colLabel,  Levels=colValues_level, Missing=colMissing, MissingType=colMissingType,Split=colSplit, ColClass=colClass)#,Align=colAlign, Measure=colMeasure,Width=colWidth, Decimals=colDecimals, Columns=colColumns, Role=colRole)
					colProperties <- list(Name=colName, Type=colType, Label=colLabel,  Levels=colValues_level, Missing=colMissing, MissingType=colMissingType, Align=colAlign, Measure=colMeasure, Split=colSplit, Width=colWidth, Decimals=colDecimals, Columns=colColumns, Role=colRole, ColClass=colClass, DateFormat =colDateFormat)
					#cat('\ncol prop list ready\n')
					#print(proc.time() - ptm)
					#- return(colProperties)
					
				}
				else
				{
					 cat("\nError: Cannot get col properties. Col not found\n")
					BSkyErrMsg =paste("UAgetColProperties: Cannot get col properties. Col not found."," Col. Name:", colNameOrIndex)
					warning("UAgetColProperties: Cannot get col properties. Col not found.")
				}				
			}
			else
			{
				#cat("\nError: Cannot get col properties. Dataset name or index not found\n")
				BSkyErrMsg = paste("UAgetColProperties: Error getting col properties. Dataset name or index not found : ", "DataSetName :", datasetname," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
				warning(BSkyErrMsg);
			}			
		#ptm= proc.time()
#BSkyFunctionWrapUp()
		#cat("\n====col======prop====S=======\n")
		#print(colProperties)
		#cat("\n====col======prop====E=======\n")
		#cat('\nBSkyFunctionWrapUp\n')
		#print(proc.time() - ptm)
	return(invisible(colProperties))
}


#incomplete. Measure not doone
#############################################################################################################
#Function: UAsetColProperties(dataSetNameOrIndex, colNameOrIndex, propertyName, propertyValue)										
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex     			- Name/index of the column whose propeties are needed 
#		propertyName				- it can be Type, Label, Missing etc..
#		propertyValue				- it is the value of the property      																
#																						
#Description:
#		Sets a specified property of apecified column of a given dataset																		
#																						
#Example: UAsetColProperties("mydataset","gender", "Label", "Male or Female")																			
#																						
#############################################################################################################
UAsetColProperties <- function(dataSetNameOrIndex, colNameOrIndex, propertyName, propertyValue, mistype="none",newOrder)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColProperties: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColProperties: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				#Error: dataSetName and colname not found
				if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
				{
					#find col name
					if(propertyName == "Name")#ok
					{										
						UAsetColNames(datasetname, colNameOrIndex, propertyValue)
						#cat("\nName:",propertyValue)
					}
					#find col type
					if(propertyName == "Type")#ok
					{					
						UAsetColType(datasetname, colNameOrIndex, propertyValue)
						#cat("\nType:",propertyValue)
					}
					#set col label
					if(propertyName == "Label")#ok
					{
						UAsetColDesc(datasetname, colNameOrIndex, propertyValue)
						#cat("\nLabel:",propertyValue)
					}
					#find col values, level part
					if(propertyName == "Levels")#ok
					{					
						UAsetColLevels(datasetname, colNameOrIndex, propertyValue)
						#cat("\nLevels:",propertyValue)
					}
					#find col missing
					if(propertyName == "Missing")#ok
					{
						UAsetColMissing(datasetname, colNameOrIndex, propertyValue, mistype)
						#cat("\nMissing:",propertyValue)
					}
					#find col measure
					if(propertyName == "Measure")
					{
						UAsetColMeasure(datasetname, colNameOrIndex, propertyValue, newOrder)#not tested properly
						#cat("\nMeasure:",propertyValue)
					}
					#Set col Alignment
					if(propertyName == "Align")
					{
						UAsetColAlign( datasetname, colNameOrIndex, propertyValue)
						#cat("\nAlignment:",propertyValue)
					}
					#find col level split
					if(propertyName == "Split")
					{
						UAsetColLevelSplit( datasetname, colNameOrIndex, propertyValue)
						#cat("\nCol Split:",propertyValue)
					}
					##################    some more prop for variable grid   ####################
					if((propertyName == "Width") || (propertyName == "Decimals") || (propertyName == "Columns"))
					{
						if(!is.na(as.numeric(propertyValue)))
						{
							propertyValue=as.numeric(propertyValue)
						}					
					}
					#find col level width
					if(propertyName == "Width")
					{
						eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$width <- ',propertyValue,sep='')))# <<- to <- coz .GlobalEnv
						#cat("\nCol Width:",propertyValue)
					}
					#find col level decimals
					if(propertyName == "Decimals")
					{
						eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$decimals <- ',propertyValue,sep='')))# <<- to <- coz .GlobalEnv
						#cat("\nCol Decimals:",propertyValue)
					}
					
					#find col level columns
					if(propertyName == "Columns")
					{
						eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$columns <- ',propertyValue,sep='')))# <<- to <- coz .GlobalEnv
						#cat("\nCol Columns:",propertyValue)															
					}
					
					if(propertyName == "DateFormat")
					{
						eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$DateFormat <- propertyValue',sep='')))# <<- to <- coz .GlobalEnv
						#cat("\nCol Role:",propertyValue)					
					}	

					#find col level splroleit ##role
					if(propertyName == "Role")
					{
						eval(parse(text=paste('attributes(',datasetname,'[,',colIndex,'])$role <- propertyValue',sep='')))# <<- to <- coz .GlobalEnv
						#cat("\nCol Role:",propertyValue)					
					}					
					
					#cat("\n\n")
					#Create two col
					##propCol1 <- c( "Name", "Type", "Label", "Factor", "Level", "Missing", "Measure" )
					##propCol2 <- c(colName, colType, colLabel, list(colValues_factor), list(colValues_level), colMissing, colMeasure)

					#Create an object with two col #removed -> Factor=colValues_factor,
					#colProperties <- list(Name=colName, Type=colType, Label=colLabel,  Values=colValues_level, Missing=colMissing, Measure=colMeasure, Split=colSplit)
					#return(colProperties)
				}
				else
				{
					# cat("\nError: Cannot set col property. Col not found\n")
					BSkyErrMsg =paste("UAsetColProperties: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
					warning("UAsetColProperties: Cannot set col property. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColProperties:  Cannot set col property. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetColProperties:  Cannot set col property. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()
		return(invisible())	
}

# excludechars: a vector of values to be excluded when forming the set of levels
BSkyMakeColumnOrderedFactor <- function(colNameOrIndex, dataSetNameOrIndex,  excludechars=c("", NA))
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyMakeColumnOrderedFactor: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyMakeColumnOrderedFactor: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
				# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
				eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- factor(x=',datasetname,'$',colNameOrIndex,',  exclude = excludechars, ordered = TRUE)', sep='')))
				# eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- factor(',datasetname,'$',colNameOrIndex,')', sep='')))# <<- to <- coz .GlobalEnv
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
		}
		else
		{
			# cat("\nError: Cannot set col property. Col not found\n")
			BSkyErrMsg =paste("BSkyMakeColumnOrderedFactor: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
			warning("BSkyMakeColumnOrderedFactor: Cannot set col property. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkyMakeColumnOrderedFactor:  Can't make it factor. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("BSkyMakeColumnOrderedFactor:  Can't make it factor. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()
		return(invisible())	
}


# excludechars: a vector of values to be excluded when forming the set of levels
BSkyMakeColumnFactor <- function(colNameOrIndex, dataSetNameOrIndex,  excludechars=c("", NA))
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyMakeColumnFactor: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyMakeColumnFactor: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
				bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
				# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
				eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- factor(x=',datasetname,'$',colNameOrIndex,',  exclude = excludechars, ordered = FALSE)', sep='')))
				# eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- factor(',datasetname,'$',colNameOrIndex,')', sep='')))# <<- to <- coz .GlobalEnv
				BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
		}
		else
		{
			# cat("\nError: Cannot set col property. Col not found\n")
			BSkyErrMsg =paste("BSkyMakeColumnFactor: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
			warning("BSkyMakeColumnFactor: Cannot set col property. Col not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkyMakeColumnFactor:  Can't make it factor. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("BSkyMakeColumnFactor:  Can't make it factor. Dataset name or index not found.")
	}			

		BSkyFunctionWrapUp()
		return(invisible())	
}


## 07Apr2017
## using a function ( BSkyMakeColumnFactor() )from column_priperties.r
## I need to loop through each variable because I need to save and restore attributes of each colum.
## If I use lapply then I will loose all those colums those were not passed to lapply for conversion.
## So I am use following method.
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Make factor variable
#'
#' @description The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors). If argument ordered is TRUE, the factor levels are assumed to be ordered. For compatibility with S there is also a function ordered.
#' is.factor, is.ordered, as.factor and as.ordered are the membership and coercion functions for these classes.
#'
#' @param colNames Variables to be converted to factor
#' @param dataSetNameOrIndex the dataset name that contains the columns those will be converted to factor
#'
#' @return
#'
#' @examples
BSkyMakeMultiColumnFactor <- function(colNames, dataSetNameOrIndex)
{
	count=length(colNames)
	for(i in 1 : count)
	{
		## all dataset and col validation are done inside following function
		BSkyMakeColumnFactor(colNames[i], dataSetNameOrIndex)
	}
	return(invisible())
}

BSkyMakeColumnString <- function(colNameOrIndex, dataSetNameOrIndex)
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyMakeColumnString: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyMakeColumnString: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				#Error: dataSetName and colname not found
				if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
				{
						bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
						# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
						eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.character(',datasetname,'$',colNameOrIndex,')', sep='')))# <<- to <- coz .GlobalEnv
						BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
				}
				else
				{
					# cat("\nError: Cannot set col property. Col not found\n")
					BSkyErrMsg =paste("BSkyMakeColumnString: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
					warning("BSkyMakeColumnString: Cannot set col property. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkyMakeColumnString:  Can't make it strin Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkyMakeColumnString:  Can't make it string. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()	
		return(invisible())
}

#11Oct2017 To make any column numeric(i.e. double)
BSkyMakeColumnNumeric <- function(colNameOrIndex, dataSetNameOrIndex)
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyMakeColumnNumeric: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyMakeColumnNumeric: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				#Error: dataSetName and colname not found
				if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
				{
					bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
					
					isfactor = eval(parse(text=paste('is.factor(',datasetname,'$',colNameOrIndex,')', sep='')))
					if(isfactor)
					{
						# this can be used if levels are numeric-strings like
						# ("3.4","5","9") and we want the same numbers after conversion i.e. 3.4, 5, 9
						# but this produces NAs if levels are like ("high", "med", "low"), i.e. pure character levels
						# eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.numeric(as.character(',datasetname,'$',colNameOrIndex,'))', sep='')))
						
						## this can be use for levels that are pure character or character-numebers
						# ("male", "female") or ("3.4","5","9") and converts to integer i.e. (1,2) and (1,2,3) respectively
						#30May2022eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.integer(',datasetname,'$',colNameOrIndex,')', sep='')))
						
						
						#30May2022 First we convert factor to character and then type.convert() should convert the column to the correct type based on the data
						# if the data in col is like ("High","med","low") it will remain character if the data is ("12","23","34") then it changes to (12,23,34)
						eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- type.convert(stringr::str_trim(as.character(',datasetname,'$',colNameOrIndex,'), side="both"),as.is=TRUE)', sep='')))
						
						##This may be used in future.
						##check if col is a character col ("Male","Female"). You can convert these levels to numeric levels (1,2)
						#ischaracter = eval(parse(text=paste('is.character(',datasetname,'$',colNameOrIndex,')', sep='')))
						#if(ischaracter)
						#{
							#eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.integer(as.factor(',datasetname,'$',colNameOrIndex,'))', sep='')))
						#}
					}
					else {
						# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
						eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.numeric(',datasetname,'$',colNameOrIndex,')', sep='')))# <<- to <- coz .GlobalEnv
					}
					BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
				}
				else
				{
					# cat("\nError: Cannot set col property. Col not found\n")
					BSkyErrMsg =paste("BSkyMakeColumnNumeric: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
					warning("BSkyMakeColumnNumeric: Cannot set col property. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkyMakeColumnNumeric:  Can't make it numeric. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkyMakeColumnNumeric:  Can't make it numeric. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()	
		return(invisible())
}


#07JAn2019 To make any column integer ( non-decimal number)
BSkyMakeColumnInteger <- function(colNameOrIndex, dataSetNameOrIndex)
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyMakeColumnInteger: Error setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyMakeColumnInteger: Warning setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
				#Error: dataSetName and colname not found
				if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
				{
						bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
						# eval(parse(text=paste(datasetname,'[,',colIndex,'] <<- factor(',datasetname,'[,',colIndex,'])', sep='')))
						eval(parse(text=paste(datasetname,'$',colNameOrIndex,' <- as.integer(',datasetname,'$',colNameOrIndex,')', sep='')))# <<- to <- coz .GlobalEnv
						BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
				}
				else
				{
					# cat("\nError: Cannot set col property. Col not found\n")
					BSkyErrMsg =paste("BSkyMakeColumnInteger: Cannot set col property. Col not found."," Col Name:", colNameOrIndex)
					warning("BSkyMakeColumnInteger: Cannot set col property. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkyMakeColumnInteger:  Can't make it integer. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkyMakeColumnInteger:  Can't make it integer. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()
		return(invisible())	
}

## Get list of all attributes/properties of all cols in the order of the cols in the dataset 

BSkyGetAllColsProperties <- function(datasetNameOrIndex, isDSValidated=FALSE)
{

attlist= list()
totcols <- eval(parse(text=paste('ncol(',datasetNameOrIndex,')')))
for (i in 1 : ncol(Dataset2))
{
 att <-UAgetColProperties(dataSetNameOrIndex=datasetNameOrIndex, colNameOrIndex=i, isDSValidated=isDSValidated)
attlist= append(attlist, list(att) )
att <- NULL
}

invisible(attlist)
}