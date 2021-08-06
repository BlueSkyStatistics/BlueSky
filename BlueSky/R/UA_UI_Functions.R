#UI layer will call functions from this file. And the subfunctions are called from these functions

######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# BSkyOpenNewDataset
# BSkyloadDataset
# BSkyReloadDataset
# BSkysaveDataset
# BSkycloseDataset
# BSkyCloseForReloading
# UAgetAllData (not complete)
# UAgetColProp
# UAsetColProp
# BSkygetIndexesOfCols
# UAgetDataByIndex (not complete)
# UAgetMissing
# UAsetMissing
# UAgetDataframeSplit
# BSkySetDataFrameSplit
# UAsetDataframeSplit
# UAremoveDataframeSplit
# BSkyOutputTestDriver
#################################################################################################

# BSkyOpenNewDataset('ds1')
#' Creates a blank dataset.
#'
#' @param datasetName is the dataset object name which points to the blank dataset that got created.
#' @export 
#' @examples
#' BSkyOpenNewDataset( 'mydata' ) # create new blank datagrid. Does not loads it in UI datagrid.
#' BSkyLoadRefreshDataframe(mydata) # to load the new dataset in the BlueSky Statistics UI datagrid
BSkyOpenNewDataset <-function(datasetName ,noOfRows=30,noOfCols=6) # rows=80 and cols=50 for electron
{
BSkyFunctionInit()
BSkySetCurrentDatasetName(datasetName)
BSkyErrMsg = paste("BSkyOpenNewDataset: Error in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
BSkyWarnMsg = paste("BSkyOpenNewDataset: Warning in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
datasetname <- datasetName
tryCatch(
{
withCallingHandlers(
{
#find extension
#extn = File.sav
if(TRUE)##filetype=="SPSS")
{
## Old code.
# var1 <- c(NA, NA)
# var2 <- c(NA, NA)
# var3 <- c(NA, NA)
# var4 <- c(NA, NA)
# var5 <- c(NA, NA)
# var6 <- c(NA, NA)

## putting NaN here shows NA in the grid but they are not NA. So converting any var to factor gives us a factor level NaN.
# var1 <- c(rep(NaN,15 )) ##c(NA, NA)
# var2 <- c(rep(NaN,15 )) ##c(NA, NA)
# var3 <- c(rep(NaN,15 )) ##c(NA, NA)
# var4 <- c(rep(NaN,15 )) ##c(NA, NA)
# var5 <- c(rep(NaN,15 )) ##c(NA, NA)
# var6 <- c(rep(NaN,15 )) ##c(NA, NA)


   ## putting 0 here shows 0 in the datagrid. They are of class numeric.
# var1 <- c(rep(0,15 ))
# var2 <- c(rep(0,15 ))
# var3 <- c(rep(0,15 ))
# var4 <- c(rep(0,15 ))
# var5 <- c(rep(0,15 ))
# var6 <- c(rep(0,15 ))
   ## putting 0 here shows 0 in the datagrid. They are of class numeric.
# var1 <- c(rep(NA,15 ))
# var2 <- c(rep(NA,15 ))
# var3 <- c(rep(NA,15 ))
# var4 <- c(rep(NA,15 ))
# var5 <- c(rep(NA,15 ))
# var6 <- c(rep(NA,15 ))

   ## 23Sep2019 Making all cells blank. Blanks turn into NA
# var1 <- c(rep("",30 ))
# var2 <- c(rep("",30 ))
# var3 <- c(rep("",30 ))
# var4 <- c(rep("",30 ))
# var5 <- c(rep("",30 ))
# var6 <- c(rep("",30 ))
## 23Sep2019 Making all cells blank. Blanks turn into NA
# DF = data.frame(A=c("","",""), B=c("","",""))

## putting NA makes vars logical and datagrid shows them as 'True'
# var1 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var2 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var3 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var4 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var5 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var6 <- c(NA, NA,NA, NA,NA, NA,NA, NA)

## putting first value zero (and rest NAs) then the whole col
# becomes numeric and shows the same in the datagrid
# var1 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var2 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var3 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var4 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var5 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var6 <- c(0, NA,NA, NA,NA, NA,NA, NA)
#df <- data.frame(a,b)
#eval( parse(text=paste(datasetname,' <<- data.frame(var1=as.character(var1),var2=as.character(var2), var3=as.character(var3), var4=as.character(var4), var5=as.character(var5), var6=as.character(var6), stringsAsFactors =FALSE)',sep='')))
# eval( parse(text=paste(datasetname,' <<- data.frame(var1=as.numeric(var1),var2=as.numeric(var2), var3=as.numeric(var3), var4=as.numeric(var4), var5=as.numeric(var5), var6=as.numeric(var6), stringsAsFactors =FALSE)',sep='')))
#eval( parse(text=paste('class(',datasetname,'$a) <<- \'character\'',sep='')))
#eval( parse(text=paste('class(',datasetname,'$b) <<-  \'character\'',sep='')))
eval(parse(text =paste (datasetName, "<<-data.frame(matrix(data = ", "rep (''," , "noOfRows*noOfCols), ncol =", noOfCols , ",nrow =", noOfRows, "),stringsAsFactors = FALSE)", sep="",collapse="")))
uadatasets$name <- c(uadatasets$name, datasetname)
}
varlbls <- c(var1="var1", var2="var2", var3="var3", var4="var4", var5="var5", var6="var6")
#cat("\nLoaded dataset :", datasetname)
DataSetIndex <- UAgetIndexOfDataSet(datasetname)
#cat("\nDatasetIndex =", DataSetIndex)
#eval(parse(text=paste('attr(',datasetname,',"variable.labels") <<- varlbls')))
#cat("\nVar lables set")
eval(parse(text=paste('attr(',datasetname,',"split") <<- c(FALSE)')))
#cat("\nsplit set.\t")
eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- c("")')))
#cat("splitcolumnnames set.\t")
eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- c("")')))
#cat("splitcolumnindex set.\t")
## Add 2 more attribute "filetype" and "slice" (differentiate slice from spss dataset)
#eval(parse(text=paste('attr(',datasetname,',"filetype") <<- c("NEW")')))
#cat("filetype set to NEW.\t")
eval(parse(text=paste('attr(',datasetname,',"slice") <<- c(FALSE)')))
#cat("slice set.\t")

### New Attribute for missings  at Dataset Level###
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- NULL')))
# cat("misval set.\t")
if(TRUE) ##is.null(missingval) )##no missing values. then set all col to "none"
{
# cat("\ncreating Missing attr.")
colcount = eval(parse(text=paste('ncol(',datasetname,')')))
for(i in 1:colcount)
{
coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
###creating missing value attribute, which is dataset level att.
colmisatt <- eval(parse(text=paste(coluname,'<-list(',coluname,'=list(type="none", value=""))')))
# print(colmisatt)
if(i>1)
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(attr(',datasetname,',"misvals"), ',colmisatt,')')))
else
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(',colmisatt,')')))
# cat("done!@")
}
}
#cat("\nCreating Extra attributes for new DS. ")
eval(parse(text=paste('attr(',datasetname,',"maxfactor") <<-', bskymaxfactors)))
UAcreateExtraAttributes(datasetname, "RDATA")
########## Following should not execute if any kind of error occur in code above #######
### Dataset already in place, now find index and do following ###
DataSetIndex <- UAgetIndexOfDataSet(datasetname)
####uadatasets$fullpath[DataSetIndex] <- fullpathfilename
#cat("\nlist of DS :")
#cat(uadatasets$name)

},
warning = UAwarnHandlerFn

) # end of withCallingHandlers for catching warnings and continuing execution
},
error = UAerrHandlerFn,
silent =TRUE
)

    if(BSkyLocalErrorFound() == TRUE)
    {
# cat("Error caught in UAloadDataset \n")
#BSkyLocalErrorFlagsReset() #if needed
    }
#cat("\nWARNING:: top level load dataset function\n")
if(BSkyLocalWarningFound() == TRUE)
    {
# cat("Warning caught in UAloadDataset \n")
BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    }
# cat("\nWrapup now")
BSkyFunctionWrapUp()
#cat("\nlist of DS finally:")
#cat(uadatasets$name)
#print(BSkyReturnStructure())
# cat("Returning return structure from this top level load dataset function\n")
return(invisible(BSkyReturnStructure()))
}


BSkyEmpty <-function(datasetName ,noOfRows,noOfCols)
{
 eval(parse(text =paste (datasetName, "<<-data.frame(matrix(data = ", "rep (''," , "noOfRows*noOfCols), ncol =", noOfCols , ",nrow =", noOfRows, "),stringsAsFactors = FALSE)", sep="",collapse=""))) 
}

###################################################################################################################
# Detail : This function can be used to load diff file formats in UA memory space
# fullpathfilename -> Full path filename(drive directory and filename) of an existing file to load the UA memory space
# datasetName -> any Name that you want as a refrence for this dataset
# worksheetName -> provide this parameter only if Excel file is to be read.
# replace_ds   -> Pass TRUE if you want to overwrite the exisitng dataset in UA memory space
# csvHeader    -> Should be TRUE if the first line of CSV file contains column name and not data.
# filetype   -> Diff file formats. Currently supports:- SPSS, XLS, XLSX, CSV, DBF, RDATA [This is case sensitive
# 				right now, provide exactly what the current support list shows]
###################################################################################################################
#' BSkyloadDataset loads a dataset into R by reading data from a disk file. When this function is executed from BlueSky R Command Editor
#' it loads the dataset in the UI datagrid.
#' [supported file types : .SAV, .XLSX, .XLS, .CSV, .SAS7BDAT, .RDATA, .DBF, and .DAT]
#'
#' @name BSkyloadDataset
#' @param fullpathfilename is a full path filename of a disk file, that is to be loaded into R. Use forward slash as a path separator.
#' @param filetype is one of the ("SPSS", "SAS7BDAT", "XLS", "XLSX", "CSV", "DBF", "RDATA" and "DAT"). Not required when executing from BlueSky R Command Editor.
#' @param worksheetName is the name of the worksheet. Used only when reading Excel file type. Not required when executing from BlueSky R Command Editor.
#' @param datasetName is a name of the object in R that corresponds to the dataset you have loaded. Not required when executing from BlueSky R Command Editor.
#'
#' Parameters replace_ds, load.missing, csvHeader are for internal use.
#' @export 
#' @examples
#' fullpathfilename <- 'C:/BlueSky Statistics/Sample Datasets/Sample R Datasets(.rdata)/caranalysis.RData'
#' filetype <- 'RDATA' #This parameter is not required when executing from BlueSky R Command Editor.
#' datasetName <- 'rdataset' #This parameter is not required when executing from BlueSky R Command Editor.
#' BSkyloadDataset(fullpathfilename=fullpathfilename)
#' BSkyLoadRefreshDataframe(testdata)
#'
#' fullpathfilename <- 'C:/BlueSky Statistics/Sample Datasets/Excel/sample.xls'
#' filetype <- 'XLS' #This parameter is not required when executing from BlueSky R Command Editor.
#' worksheetName = 'Sheet1' #This parameter is not required when executing from BlueSky R Command Editor.
#' datasetName <- 'exceldata' #This parameter is not required when executing from BlueSky R Command Editor.
#' BSkyloadDataset(fullpathfilename=fullpathfilename)
#' BSkyLoadRefreshDataframe(exceldata)
BSkyloadDataset <-function(fullpathfilename,  filetype, worksheetName=NULL, replace_ds=FALSE, 
load.missing = FALSE, csvHeader=TRUE,character.to.factor=FALSE, isBasketData=FALSE, trimSPSStrailing=FALSE, sepChar=',', deciChar='.', datasetName )
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetName)
	
	BSkyErrMsg = paste("BSkyloadDataset: Error in Loading dataset : ", "DataSetName :", datasetName," ", "Filepath :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyloadDataset: Warning in Loading dataset : ", "DataSetName :", datasetName," ", "Filepath :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	datasetname <- datasetName
	tryCatch(
		{
		withCallingHandlers(
		{
			#cat("Top Level - Start Loading Dataset:",datasetname)
			#print(Sys.time())
			#find extension
			#extn = File.sav
			if(filetype=="SPSS"){
				UAloadSPSSinDataFrame(fullpathfilename, datasetname, replace=replace_ds, loadMissingValue=load.missing, trimSPSStrailing=trimSPSStrailing) 
			}
			if(filetype=="SAS7BDAT"){
				BSkyLoadSASinDataFrame(fullpathfilename, datasetname, replace=replace_ds) 
			}			
			else if(filetype == "XLS"){
				UAreadExcel(fullpathfilename, datasetname, worksheetName, replace=replace_ds, xlsx=FALSE, character.to.factor=character.to.factor)
			}
			else if(filetype == "XLSX"){
				UAreadExcel(fullpathfilename, datasetname, worksheetName, replace=replace_ds, xlsx=TRUE, character.to.factor=character.to.factor)
			}			
			else if(isBasketData && (filetype == "CSV" || filetype == "DAT" || filetype == "TXT") ) #if its mkt basket data in a flat file having extension csv, txt or data
			{
					# For Basket data (transaction in one line) some separator will be used
					# but to load that data we must ignore that separator if we want to read
					# each line in a separate row of a single column(say Items)
					# User data file can contain a separator from a vast range of characters 
					# but not the vertical tab character. We want our sepChar to not match to any
					# separator available to user. Appropriate candidate is vertical tab.
					sepChar='\v' #vertical tab should never be found as a separator in user data file.
				BSkyLoadDATinDataFrame(fullpathfilename, datasetname, replace=replace_ds, Header=csvHeader, sepCh=sepChar, deciCh=deciChar)			
			}
			else if(filetype == "CSV"){

				UAreadCSV(fullpathfilename, datasetname, Header=csvHeader, replace=replace_ds, character.to.factor=character.to.factor, sepCh=sepChar, deciCh=deciChar)
			}
			else if(filetype == "DBF"){
				UAreadDBF(fullpathfilename, datasetname,replace=replace_ds)
			}
			else if(filetype == "RDATA" || filetype == "RDA"){ ##this block only runs when BSkyReloadDataset() is run on RData with single dataframe object.
			## so load.refresh is needed below.

				#31May2018 UAreadRObj(fullpathfilename, datasetname, replace=replace_ds)
				#BSky.GetDataframeObjNames(RObjFilename=fullpathfilename, ReturnObjectName='BSkyCurrentRObj')
				#BSky.LoadRefresh.Dataframe(dframe)
				
				#19Oct2019 above 2 line does not seem to work so code below should just work fine for single dataset RDATA file
				eval(parse(text=paste('UAreadRObj(RObjfileName="',fullpathfilename,'", datasetname="',datasetname,'", replace=TRUE)',sep='')))
			}			
			else if(filetype=="DAT"){
				BSkyLoadDATinDataFrame(fullpathfilename, datasetname, replace=replace_ds, Header=csvHeader, sepCh=sepChar, deciCh=deciChar) 
			}					
			else  if(filetype == "TXT"){
				BSkyLoadDATinDataFrame(fullpathfilename, datasetname, replace=replace_ds, Header=csvHeader, sepCh=sepChar, deciCh=deciChar) 
			}
			#cat("Top Level - Finished Loading Dataset:",datasetname)
			#print(Sys.time())
			# if ( isUniqueColumns(datasetname) )
			# {
				########## Following should not execute if any kind of error occur in code above #######
				### Dataset already in place, now find index and do following ###
				DataSetIndex <- UAgetIndexOfDataSet(datasetname)
				uadatasets$fullpath[DataSetIndex] <- fullpathfilename
				#print(eval(parse(text=paste('colnames(',datasetname,')'))))
				replacePeriodWithUnderscore(datasetname)	
				#print(eval(parse(text=paste('colnames(',datasetname,')'))))
			# }
			# else
			# {
				# cat("\nStopping!")
				# #BSkyRemoveRefreshDataframe(datasetname)
				# stop("The dataset has duplicate column names.")
				# cat("\nStopped!")
			# }
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
			
			# cat("Error caught in UAloadDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level load dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAloadDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		# cat("\nWrapup now")
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		# cat("Returning return structure from this top level load dataset function\n")
		return(invisible(BSkyReturnStructure2()))
}

#Reloads data along with attributes. If you modified data and want to get original data back(reload from disk).
#For CSV and EXCEL this may not work because of extra parameter required. Once proper values are provided to those it should work.
## Loads dataset from disk file.
BSkyReloadDataset<-function(fullpathfilename,  filetype, sheetname=NULL, csvHeader=TRUE, loaddataonly=FALSE,isBasketData=FALSE, sepChar=',', deciChar='.', datasetname)
{
	#cat("\nReloading Dataset...");
	#print(loaddataonly)
	#cat("\n");
	
	#if someone runs realod on dataset that is reduced few rows and then loaddataonly=TRUE,
	#other attributes will not be created and there will be problem loading variable grid. 
	if(FALSE) #(loaddataonly) ## reloads data only#
	{
			## Dataset level attributes are backed up ( Do backup column level attribute too for each column )
			bkupattrs <- eval(parse(text=paste('attributes(',datasetname,')')))
			#cat('\nDataset opened with  name\n',datasetname)
			if(filetype=="SPSS"){
				eval( parse(text=paste(datasetname,' <<- read.spss(file=\'',fullpathfilename,'\',use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)',sep='')))
			}
			else if(filetype == "XLS"){
				channel<-NULL
				channel <- odbcConnectExcel(fullpathfilename, readOnly=FALSE)
				eval( parse(text=paste(datasetname,' <<- sqlQuery(channel, \'select * from [',sheetname,'$]\')', sep=''  )))
				odbcCloseAll()
			}
			else if(filetype == "XLSX"){
				channel<-NULL
				channel <- odbcConnectExcel2007(fullpathfilename, readOnly=FALSE)
				eval( parse(text=paste(datasetname,' <<- sqlQuery(channel, \'select * from [',sheetname,'$]\')', sep=''  )))
				odbcCloseAll()
			}			
			else if(filetype == "CSV"){
				eval( parse(text=paste(datasetname,' <<- read.csv(\'',fullpathfilename,'\', header=Header)',sep='')))
			}
			else if(filetype == "DBF"){
				eval( parse(text=paste(datasetname,' <<- read.dbf(\'',fullpathfilename,'\')', sep='' )))
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
					### Following structure is already created when saving a file. While load robj file these are filled automatically ####
					UAObj <- NULL
					UAObj$info <- NULL
					UAObj$obj <- NULL
					# load(file = fullpathfilename)#, uadatasets$lst[Newindex])
					# #Check signature. If rdata file is created using BSky software then only open it
					# bskysig <- UAObj$info
					# if(!is.null(bskysig) && bskysig$ver==1.2 && bskysig$name=="BSky")
					# {
						# eval( parse(text=paste( datasetname,' <<- UAObj$obj' , sep=''   )))
					# }				
					
					## above code commented
					## 31May 2018 loading RData file that has single dataset only and no other object.
					## it could be of UAObj format or just one data.frame saved in RData file.
					#load(file = fullpathfilename, envir = .GlobalEnv)
					
					#17Oct2019
					eval(parse(text=paste('UAreadRObj(RObjfileName="',fullpathfilename,'", datasetname="',datasetname,'", replace=TRUE)',sep='')))
			}			
			else  if(filetype == "TXT"){
			}	
			###restoring Dataset level attributes (Do restore column level attributes for each column too)
			eval(parse(text=paste('attributes(',datasetname,') <<- bkupattrs')))
	}
	else ## reloads everything, data attributes etc.
	{
		BSkyloadDataset(fullpathfilename,  filetype, worksheetName=sheetname, replace_ds=TRUE, csvHeader=csvHeader, isBasketData=isBasketData, sepChar=sepChar, deciChar=deciChar,datasetName=datasetname )
	}
	return(invisible(NULL))
}


###################################################################################################################
# fullpathfilename -> Full path filename(drive directory and filename) of an existing file to load the UA memory space
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# rownames			-> For Excel only. Serial number for each record
# colnames			-> For Excel only.
# newWorksheetName -> For Excel only (filetype must be XLS or XLSX).New Worksheet name where dataset is to be saved
# 					  
# fact3char			- For DBF only, Factor col are written in character if TRUE. Else they will contain integer 
# filetype -> Case sensitive only supports SPSS, XLS, XLSX, DBF, CSV, RDATA
###################################################################################################################

#' BSkysaveDataset saves a BlueSky Statistics UI datagrid loaded dataset to the supported file formats.
#' [supported file formats : .XLSX, .CSV, .RDATA, and .DBF]
#' @name BSkysaveDataset
#' @param fullpathfilename is a full path filename of a disk file to which will dataset will be written. Use forward slash as path separator.
#' @param filetype is the file format you want to save to. It must be one of the ("XLSX", "CSV", "DBF", "RDATA").
#' @param worksheetName is the name of the excel worksheet. Used only when saving to an Excel file format.
#' @param dataSetNameOrIndex is a name of the dataset that already loaded in the BlueSky Statistics UI datagrid. Use single quotes around dataset name.
#' 
#' Parameters Rownames, Colnames, newWorksheetName, factor2char are for internal use.
#' @export 
#' @examples
#' df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
#' BSkyLoadRefreshDataframe(df) #dataframe must be loaded in the grid before you try to save it to different formats.
#' BSkysaveDataset("D:/mytestdata.rdata", filetype="RDATA", dataSetNameOrIndex='df')
BSkysaveDataset <-function(fullpathfilename,  filetype, Rownames = TRUE, Colnames = FALSE, newWorksheetName=NULL,factor2char=TRUE, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsaveDataset: Error in Saving dataset : ", "DataSetName :", dataSetNameOrIndex," ", "Filename :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsaveDataset: Warning in Saving dataset : ", "DataSetName :", dataSetNameOrIndex," ", "Filename :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
		withCallingHandlers(
		{
			#find extension
			#extn = File.sav
			if(filetype=="SAV"){
				BSkywriteSAV(fullpathfilename, dataSetNameOrIndex)  #requires rio R package
			}
			else if(filetype == "XLS"){
				UAwriteExcel(fullpathfilename, dataSetNameOrIndex, newWorksheetName, row.names = Rownames, col.names = Colnames, xlsx=FALSE)
			}
			else if(filetype == "XLSX"){
				UAwriteExcel(fullpathfilename, dataSetNameOrIndex, newWorksheetName, row.names = Rownames, col.names = Colnames, xlsx=TRUE)
			}			
			else if(filetype == "CSV"){
				UAwriteCSV(fullpathfilename, dataSetNameOrIndex)
			}
			else if(filetype == "DBF"){
				UAwriteDBF(fullpathfilename, dataSetNameOrIndex, fact2char=factor2char)
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
				UAwriteRObj(fullpathfilename,dataSetNameOrIndex)
			}			
			else  if(filetype == "TXT"){
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
			
			# cat("Error caught in UAsaveDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level save dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsaveDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level save dataset function\n")
		return(invisible(BSkyReturnStructure()))
}

# "UAsaveDataset('F:/SPSS/d2.xls', 'Dataset1',  newWorksheetName='Sheet1', filetype='XLS')"


###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already assigned to the dataset while reading/loading. Or index of the dataset
# This dataset name could be in-memory dataframe (created on the fly). Or it could be, dataframe created by loading (say SPSS) file from disk
# Dataframe is in memory, the difference is how its created.
###################################################################################################################

#' BSkycloseDataset closes a dataset, removing it from the R.
#' @name BSkycloseDataset
#' @param dataSetNameOrIndex is the name of the dataset that is to be closed.
#' @export 
#' @examples
#' df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
#' BSkycloseDataset(df)
BSkycloseDataset<-function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAcloseDataset: Error in Closing Dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyWarnMsg = paste("UAcloseDataset: Warning in Closing Dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
			
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			if(!is.null(datasetname))
			{
				#Setting DataSetIndex location in uadatasets to NULL, to remove it.
				eval(parse(text=paste(datasetname, '<- NULL')))
				#cat("\nDataset closed by setting it to null\n")
				## Also need to remove this name from uadatasets$name list
				# uadatasets$name <- names(uadatasets$lst)
				
				#19Oct2016 This call is just for testing. And can also be replaced for the next UAgetIndexOfDataSet call
				# idx = UAgetIndexOfDataSet(datasetname)
				# cat('\n.GlobalEnv passed : Index of the dataset:\n')
				# print(idx)
				
				#19Oct2016 only pass dataset name (eg.. 'Dataset1') and not '.GlobalEnv$Dataset1'
				#20Oct2016 Since a fix has been made to UAgetIndexOfDataSet so you can pass 'Dataset1' or '.GlobalEnv$Dataset1'
				#The code commented above uses '.GlobalEnv$Dataset1' and works fine too.
				idx = UAgetIndexOfDataSet(dataSetNameOrIndex) 
				#cat('\nNo .GlobalEnv: Index of the dataset:\n')
				#print(idx)
				
				if(idx > 0) #19Oct2016 do not use this --> if(length(idx) > 0)
				{
					uadatasets$name[idx] <- "" ## making the closed dataset blank. So index of existsing open dataset will not change.
					dscount =  length(uadatasets$name)
					#cat('\nTotal DS\n')
					#print(dscount)
					oldds <- uadatasets$name
					uadatasets$name <- NULL
					for(i in 1 : dscount)
					{
						#cat("\n", oldds[i], "\t", nchar(oldds[i]))
						if(nchar(oldds[i]) > 0)
						{
							uadatasets$name <- c(uadatasets$name, oldds[i])
							#cat("\n+++", oldds[i])
						}
					}
					
				}
				#-return(NULL)
				eval(parse(text=paste('remove(',dataSetNameOrIndex, ', envir=.GlobalEnv)')))
			}
			else
			{
				BSkyErrMsg =paste("UAcloseDataset: Cannot close dataset. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				# cat("\nError: Cannot close dataset. Dataset name or index not found\n")
				warning("UAcloseDataset: Cannot close dataset. Dataset name or index not found.")
				#-return(NULL)
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
			
			# cat("Error caught in UAcloseDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level close dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAcloseDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level close dataset function\n")
		return(invisible(BSkyReturnStructure()))
}

# closes dataset with name 'datasetname'. Following function only closes and does not perform reopening.
# ReOpen logic should reside in the parent function within which following function is called (to first close and then reopen).
BSkyCloseForReloading<-function(datasetname)
{
			if(!is.null(datasetname))
			{
				#Setting DataSetIndex location in uadatasets to NULL, to remove it.
				eval(parse(text=paste(datasetname, '<- NULL')))
				#cat("\nDataset closed by setting it to null\n")
				## Also need to remove this name from uadatasets$name list
				# uadatasets$name <- names(uadatasets$lst)
				
				idx = UAgetIndexOfDataSet(datasetname)
				if(length(idx) > 0)
				{
					uadatasets$name[idx] <- "" ## making the closed dataset blank. So index of existsing open dataset will not change.
					dscount =  length(uadatasets$name)
					oldds <- uadatasets$name
					uadatasets$name <- NULL
					for(i in 1 : dscount)
					{
						#cat("\n", oldds[i], "\t", nchar(oldds[i]))
						if(nchar(oldds[i]) > 0)
						{
							uadatasets$name <- c(uadatasets$name, oldds[i])
							#cat("\n+++", oldds[i])
						}
					}
				}
				eval(parse(text=paste('remove(',datasetname, ', envir=.GlobalEnv)')))
			}
}
# not complete
UAgetAllData<-function()
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetAllData: Error in get all data : ",  sep="")
	BSkyWarnMsg = paste("UAgetAllData: Warning in get all data : ",  sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
		withCallingHandlers(
		{
DataSetIndex <- BSkyValidateDataset(dataSetNameOrIndex)		
			filetype <- attr(uadatasets$lst[[DataSetIndex]],"filetype")
			#cat("\nFile Typ:",filetype,"\n")	
			
			if(filetype=="SPSS"){
					
			}
			else if(filetype == "XLS"){
			}
			else if(filetype == "XLSX"){
			}			
			else if(filetype == "CSV"){
			}
			else if(filetype == "DBF"){
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
			}			
			else if(filetype == "TXT"){
			}			
			else{
				warning("UAgetAllData: File type not supported.")
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
			
			# cat("Error caught in UAgetAllData \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get all data function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetAllData \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get all data function\n")
		#return(BSkyReturnStructure())	
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the column whose properties you need. eg.. "gender"
# propname			 -> Name of the property whose value is needed. For 'All' all the properties will be returned
###################################################################################################################
UAgetColProp<-function(colNameOrIndex, propname='All', Type.as.Class=FALSE, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetColProp: Error in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetColProp: Warning in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	prop <- list()
	tryCatch(
		{

		withCallingHandlers(
		{
			if(propname == 'All')
			{
				prop<-UAgetColProperties(dataSetNameOrIndex, colNameOrIndex, asClass=Type.as.Class)
				 #cat("\nCol Properties:\n")
				 #print(prop)
				 #cat("\n")
			}
			else
			{
				prop<-UAgetColProperties(dataSetNameOrIndex, colNameOrIndex, asClass=Type.as.Class)
				#cat("\nAll Properties:",prop[[2]],"\n")
				prop <- eval(parse(text = paste('prop$', propname, sep = '', collapse=',' )))
				#cat("\nSingle Property:",prop,"\n")
				# eval(parse(text = paste(datasetName,'$', datasetVariableList[i])))
			}
			#-return(prop)
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
			
			# cat("Error caught in UAgetColProp \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get col property function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetColProp \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAgetColProp
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get col property function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(prop)))))  
}


UAsetColProp<-function(colNameOrIndex, propertyName, propertyValue, mistype="none", newOrder=c(), dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColProp: Error in setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColProp: Warning in setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success <- -1
	tryCatch(
		{
	
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)	
			
			if(!is.null(datasetname))
			{		
				# filetype <- attr(uadatasets$lst[[DataSetIndex]],"filetype")
				# cat("\nFTyp:",filetype,"\n")
			    UAsetColProperties(dataSetNameOrIndex, colNameOrIndex, propertyName, propertyValue, mistype, newOrder)
				success <- 1 #- return (1)
			}
			else
			{
				BSkyErrMsg =paste("UAsetColProp: Cannot set col property. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				warning("UAsetColProp: Cannot set col property. Dataset name or index not found.")
				success <- -1 #return (-1)
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
			
			# cat("Error caught in UAsetColProp \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set clo properties function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetColProp \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set clo properties function\n")
		return(invisible(BSkyReturnStructure())) #return(success)
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNames -> name of the columns whose indexes you need. eg..  c("age", "gender")
###################################################################################################################
BSkygetIndexesOfCols<-function(colNames, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkygetIndexesOfCols: Error in get cols indexes : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkygetIndexesOfCols: Warning in get cols indexes : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	indexes <- -1
	tryCatch(
		{
		
		withCallingHandlers(
		{
	
			indexes<-UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,colNames)
			#- return(indexes)
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
			
			# cat("Error caught in BSkygetIndexesOfCols \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get cols indexes function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkygetIndexesOfCols \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get cols indexes function\n")
		return(invisible(indexes)) #return(BSkyReturnStructure(indexes))
}

# not comlpete
UAgetDataByIndex<-function(filetype)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetDataByIndex: Error in get data by index : ", "DataSetName :", filetype," ", "Variable Name List :", paste(filetype, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetDataByIndex: Warning in get data by index : ", "DataSetName :", filetype," ", "Variable Name List :", paste(filetype, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
					
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				filetype <- eval(parse(text=paste('attr(',datasetname,',"filetype")',sep='')))
				#cat("\nFTyp:",filetype,"\n")
				
				if(filetype=="SPSS"){
						# indexes<-UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,colNames)
				}
				else if(filetype == "XLS"){
				}
				else if(filetype == "XLSX"){
				}	
				else if(filetype == "CSV"){
				}
				else if(filetype == "DBF"){
				}
				else if(filetype == "RDATA" || filetype == "RDA"){
				}
				else if(filetype == "TXT"){
				}

			}
			else
			{
				# cat("\nError: Cannot get data by index. Dataset name or index not found\n")
				warning("UAgetDataByIndex: Cannot get data by index. Dataset name or index not found.")
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
			
			# cat("Error caught in UAgetDataByIndex \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get data by index function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetDataByIndex \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get data by index function\n")
		return(invisible(BSkyReturnStructure()))
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the columns whose missing values you need. eg..   "age"
###################################################################################################################
UAgetMissing<-function( colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetMissing: Error in get missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetMissing: Warning in get missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	miss <- ""
	tryCatch(
		{

		withCallingHandlers(
		{

			miss<-UAgetColMissing(dataSetNameOrIndex, colNameOrIndex)
			#- return(miss)
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
			
			# cat("Error caught in UAgetMissing \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get Missing function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetMissing \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get Missing function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(miss))))) #return(miss)
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the columns whose missing values you want to set. eg..   "age"
# missvals -> vector representing missing values. eg.. c(100, 200,300)
###################################################################################################################
UAsetMissing<-function( colNameOrIndex, missvals=NULL, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetMissing: Error in setting missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetMissing: Warning in setting missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
		withCallingHandlers(
		{
		
			UAsetColMissing(dataSetNameOrIndex, colNameOrIndex, misvals=missvals)	
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
			
			# cat("Error caught in UAsetMissing \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		# cat("\nWARNING:: top level set missing function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetMissing \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set missing function\n")
		return(invisible(BSkyReturnStructure()))
}

#################################  NOT IN USE ###### (better check)  ############################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
###################################################################################################################
UAgetDataframeSplit<-function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetDataframeSplit: Error in getting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ", sep="")
	BSkyWarnMsg = paste("UAgetDataframeSplit: Warning in getting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	splitprop <-""
	tryCatch(
		{
	
		withCallingHandlers(
		{

			splitprop<-UAgetDataframeSplitProp(dataSetNameOrIndex)	
			#- return(splitprop)
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
			
			# cat("Error caught in UAgetDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get dataframe split function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get dataframe split function\n")
		return(invisible(splitprop)) # return(BSkyReturnStructure(splitprop))
}

BSkySetDataFrameSplit <- function(col.names, datasetnameorindex, removeall.splits=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("BSkySetDataFrameSplit: Error in setting datafram split : ", "DataSetName :", datasetnameorindex," ", "Variable Name List :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkySetDataFrameSplit: Warning in setting datafram split : ", "DataSetName :", datasetnameorindex," ", "Variable Name List :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
	
		withCallingHandlers(
		{
 if(!is.na(col.names) && length(col.names) > 0) #not NA and there is alteast one col name.
 {
	 #cat("\nSetting-Split:\n")
	# UAsetDataframeSplit(col.names, datasetnameorindex)
	UAsetDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names)
}
 else
 {
	 #cat("\n Removing-Split:\n")
	# UAremoveDataframeSplit( col.names, removeall.splits=TRUE, datasetnameorindex)
	if(removeall.splits || is.null(col.names) || is.na(col.names) || length(col.names)==0)
		removeallsplits = TRUE
	UAremoveDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names, removeAllSplits=removeallsplits)
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
			
			# cat("Error caught in BSkySetDataFrameSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set dataframe split function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkySetDataFrameSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set dataframe split function\n")
		invisible(BSkyReturnStructure(bskyAdditionalTableList = NA))
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colName_s  -> one or more column names for those you want to set split. Sequence is important. 
# 				eg.. c("gender", "tg1", "wgt2")
###################################################################################################################
UAsetDataframeSplit<-function(col.names, datasetnameorindex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("UAsetDataframeSplit: Error setting dataframe split : ", "DataSetName :", datasetnameorindex," ", "Variable  :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetDataframeSplit: Warning setting dataframe split : ", "DataSetName :", datasetnameorindex," ", "Variable :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
		tryCatch(
		{

		withCallingHandlers(
		{
			UAsetDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names)
			#print(attributes(accidents)$split)
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
			
			# cat("Error caught in UAsetDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		invisible(BSkyReturnStructure()) #		
}


###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colName_s  -> one or more column names for those you want to set split. Sequence is important. 
# 				eg.. c("gender", "tg1", "wgt2")
###################################################################################################################
UAremoveDataframeSplit<-function( col.names, removeall.splits=FALSE, datasetnameorindex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("UAremoveDataframeSplit: Error  : ", "DataSetName :", datasetnameorindex," ", "Variable  :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAremoveDataframeSplit: Warning  : ", "DataSetName :", datasetnameorindex," ", "Variable :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
	
		withCallingHandlers(
		{
			UAremoveDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names, removeAllSplits=removeall.splits)
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
			
			# cat("Error caught in UAremoveDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAremoveDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure())) #
}


###################################################################################################################
#Empty function for testing output templates
###################################################################################################################
BSkyOutputTestDriver<-function(colNameOrIndex=NA,dataSetNameOrIndex=NA)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyOutputTestDriver: Error output test driver : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyOutputTestDriver: Warning output test driver : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{

		withCallingHandlers(
		{
			cat("\nEmpty Function Executed!\n")
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
			
			# cat("Error caught in BSkyOutputTestDriver \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyOutputTestDriver \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure()))#
}


#starttime =Sys.time()
#test =uaonesmt.test(vars,mu=0,conf.level=0.95,datasetname,missing=1)
#time =Sys.time() -starttime
#test
#time

#UAloadDataset("F:/BlueSky/Projects/SPSS/dietstudy.sav",  filetype="SPSS", worksheetName=NULL, replace_ds=FALSE, csvHeader=TRUE, datasetName="diet" )