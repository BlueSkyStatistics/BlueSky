
BSkySortDatagridCol <- function(colNameOrIndex, sortorder="asc", dataSetNameOrIndex, isDSValidated=FALSE)
{
	BSkyLoadRefreshDataframe(dataSetNameOrIndex)				

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkySortDatagridCol: Error sorting column : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkySortDatagridCol: Warning sorting column : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
		colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)			
		#Error: dataSetName and colname not found
		if(colIndex > 0)  ##There is no check for property name. As, from UI noboby can send invalid property name
		{
				
			bskyattrs <- BSkyAttributesBackup(colIndex, datasetname) ## backup existing attributes
						
			require(dplyr);
			if(sortorder=="asc")
			{
			#Dataset10  <-Dataset10 %>% arrange(air_time)
			eval(parse(text=paste(datasetname,'<-', datasetname,' %>% arrange(',colNameOrIndex,')'))) #Sort ascending
			}
			else
			{
			#Dataset10  <-Dataset10 %>% arrange(desc(air_time))
			eval(parse(text=paste(datasetname,'<-', datasetname,' %>% arrange(desc(',colNameOrIndex,'))'))) #Sort ascending
			}						

			BSkyAttributesRestore(colIndex, bskyattrs, datasetname)## restore all attributes
		}
		else
		{
			# cat("\nError: Cannot set col property. Col not found\n")
			BSkyErrMsg =paste("BSkySortDatagridCol: Cannot sort column. Column not found."," Col Name:", colNameOrIndex)
			warning("BSkySortDatagridCol: Cannot sort column. Column not found.")
		}				
	}
	else
	{
		# cat("\nError: Cannot set col property. Dataset name or index not found\n")
		BSkyErrMsg =paste("BSkySortDatagridCol:  Cannot sort column. Dataset name or index not found."," Dataset Name:", datasetname)
		warning("BSkySortDatagridCol:  Cannot sort column. Dataset name or index not found.")
	}			

	BSkyFunctionWrapUp()	
}